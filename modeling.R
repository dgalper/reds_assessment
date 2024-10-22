# Load in packages
library(tidyverse)
library(compositions)
library(zoo)
library(caTools)

# Read in the data
pitch_df <- read_csv("data.csv")

# Define vectors for each pitch category (fastballs, off-speed, and breaking balls)
fastball_vec <- c("FF", "FC", "SI")

off_speed_vec <- c("CH","EP","FS", "KN")

breaking_ball_vec <- c("CS", "CU", "FO", "KC", "SC", "SL", "ST", "SV")

# Filter out pitchouts, pitches classified as other, and pitch types with NA
# Map pitch types to fastballs, off-speed, and breaking balls
pitch_df <- pitch_df %>%
  filter(!PITCH_TYPE %in% c("PO", "FA"),
         !is.na(PITCH_TYPE)) %>%
  mutate(fastball = PITCH_TYPE %in% fastball_vec,
         off_speed = PITCH_TYPE %in% off_speed_vec,
         breaking_ball = PITCH_TYPE %in% breaking_ball_vec)

# We are going to use data from 2021 and 2022 to predict pitch mixes in 2023
# Then, we will use that model to predict 2024 pitch mixes with 2022 and 2023 data

# Get batter pitch mixes from 2023
# Filter to 2023
pitch_df_23 <- pitch_df %>%
  filter(GAME_YEAR == 2023)

# Aggregate 2023 batter pitch mixes and plate appearances
pitch_mixes_23 <- pitch_df_23 %>%
  group_by(BATTER_ID, PLAYER_NAME) %>%
  summarize(fastball_pct = mean(fastball),
            off_speed_pct = mean(off_speed),
            breaking_ball_pct = mean(breaking_ball),
            pitches_faced = n()) %>%
  ungroup()

# Let's get batter pitch mixes from 2021 and 2022
# Filter to 2021 and 2022
pitch_df_21_22 <- pitch_df %>%
  filter(GAME_YEAR < 2023)

# We want to get the percentages across a batter's 3000 most recent pitches
# Define the size of the rolling window
rolling_window_size <- 3000

# Add in rolling percentages of each pitch type for each player
rolling_pitch_mixes_21_22 <- pitch_df_21_22 %>%
  group_by(BATTER_ID,PLAYER_NAME) %>%
  mutate(rolling_fastball_pct = coalesce(rollapply(fastball, width = rolling_window_size, FUN = mean, fill = NA, align = "right"),
                                    cummean(fastball)),
         rolling_off_speed_pct = coalesce(rollapply(off_speed, width = rolling_window_size, FUN = mean, fill = NA, align = "right"),
                                     cummean(off_speed)),
         rolling_breaking_ball_pct = coalesce(rollapply(breaking_ball, width = rolling_window_size, FUN = mean, fill = NA, align = "right"),
                                         cummean(breaking_ball)),
         pitches_faced = row_number()) %>%
  ungroup()

# Get the final rolling percentages (last 3000 or up to 3000 pitches)
pitch_mixes_21_22 <- rolling_pitch_mixes_21_22 %>%
  group_by(BATTER_ID,PLAYER_NAME) %>%
  summarize(rolling_fastball_pct = last(rolling_fastball_pct),
            rolling_off_speed_pct = last(rolling_off_speed_pct),
            rolling_breaking_ball_pct = last(rolling_breaking_ball_pct),
            total_pitches_faced = max(pitches_faced)) %>%
  ungroup()

# Join in pitch mixes from 2023 (what we are trying to predict)
# Filter to batters who saw at least 1000 pitches
pitch_mixes_21_22_23 <- pitch_mixes_21_22 %>%
  left_join(pitch_mixes_23, by = c("BATTER_ID", "PLAYER_NAME")) %>%
  filter(pitches_faced >= 1000) %>%
  mutate(total_pct = fastball_pct + off_speed_pct + breaking_ball_pct,
         fastball_pct = fastball_pct / total_pct,
         off_speed_pct = off_speed_pct / total_pct,
         breaking_ball_pct = breaking_ball_pct / total_pct)

# Set seed for reproducibility
set.seed(123)

# Split the data into training (80%) and testing (20%) sets
split <- sample.split(pitch_mixes_21_22_23$fastball_pct, SplitRatio = 0.8)

# Create training and testing sets
train_data <- subset(pitch_mixes_21_22_23, split == TRUE)
test_data <- subset(pitch_mixes_21_22_23, split == FALSE)

# Fit the compositional regression model on the training set
Y_train <- acomp(train_data %>%
                   select(fastball_pct, off_speed_pct, breaking_ball_pct))

# Fit the model using training data predictors
comp_model_train <- lm(Y_train ~ rolling_fastball_pct + rolling_off_speed_pct + rolling_breaking_ball_pct + total_pitches_faced, 
                       data = train_data)

# Make predictions on the test set
predictions_test <- predict(comp_model_train, newdata = test_data)

# Convert predictions to a data frame to merge with the test set
predicted_df_test <- as.data.frame(predictions_test)
colnames(predicted_df_test) <- c("pred_fastball_pct", "pred_off_speed_pct", "pred_breaking_ball_pct")

# Add the predictions back to the test set
test_data <- cbind(test_data, predicted_df_test)

# Calculate MAE for fastball percentage predictions
mae_fastball <- mean(abs(test_data$fastball_pct - test_data$pred_fastball_pct))
mae_off_speed <- mean(abs(test_data$off_speed_pct - test_data$pred_off_speed_pct))
mae_breaking_ball <- mean(abs(test_data$breaking_ball_pct - test_data$pred_breaking_ball_pct))

# Print MAEs
print(paste("Fastball MAE:", mae_fastball))
print(paste("Off-Speed MAE:", mae_off_speed))
print(paste("Breaking Ball MAE:", mae_breaking_ball))

  
# Let's prep the data for the actual predictions
# Let's get batter pitch mixes from 2022 and 2023
# Filter to 2022 and 2023
pitch_df_22_23 <- pitch_df %>%
  filter(GAME_YEAR >= 2022)

# Calculate rolling percentages of each pitch type for each player
rolling_pitch_mixes_22_23 <- pitch_df_22_23 %>%
  group_by(BATTER_ID,PLAYER_NAME) %>%
  mutate(rolling_fastball_pct = coalesce(rollapply(fastball, width = rolling_window_size, FUN = mean, fill = NA, align = "right"),
                                    cummean(fastball)),
         rolling_off_speed_pct = coalesce(rollapply(off_speed, width = rolling_window_size, FUN = mean, fill = NA, align = "right"),
                                     cummean(off_speed)),
         rolling_breaking_ball_pct = coalesce(rollapply(breaking_ball, width = rolling_window_size, FUN = mean, fill = NA, align = "right"),
                                         cummean(breaking_ball)),
         pitches_faced = row_number()) %>%
  ungroup()

# Get the final rolling percentages (last 1000 or up to 1000 pitches)
pitch_mixes_22_23 <- rolling_pitch_mixes_22_23 %>%
  group_by(BATTER_ID,PLAYER_NAME) %>%
  summarize(rolling_fastball_pct = last(rolling_fastball_pct),
            rolling_off_speed_pct = last(rolling_off_speed_pct),
            rolling_breaking_ball_pct = last(rolling_breaking_ball_pct),
            total_pitches_faced = max(pitches_faced)) %>%
  ungroup()

# Get 2024 predictions
predictions_2024 <- predict(comp_model_train, newdata = pitch_mixes_22_23)

# Add the predictions to the df
predicted_df_2024 <- as.data.frame(predictions_2024)
colnames(predicted_df_2024) <- c("pred_fastball_pct", "pred_off_speed_pct", "pred_breaking_ball_pct")

# Add the predictions back to the test set
matched_predictions_24 <- cbind(pitch_mixes_22_23, predicted_df_2024) %>%
  select(BATTER_ID, PLAYER_NAME, 
         PITCH_TYPE_FB = pred_fastball_pct, 
         PITCH_TYPE_OS = pred_off_speed_pct, 
         PITCH_TYPE_BB = pred_breaking_ball_pct)

# Read in the empty predictions csv
empty_predictions_df <- read_csv("predictions.csv") %>%
  select(BATTER_ID, PLAYER_NAME, GAME_YEAR)

# FIll out the predictions df
filled_predictions_df <- empty_predictions_df %>%
  left_join(matched_predictions_24, by = c("BATTER_ID", "PLAYER_NAME"))

# Save filled out predictions df to csv
write.csv(filled_predictions_df, "filled_predictions.csv", row.names = F)
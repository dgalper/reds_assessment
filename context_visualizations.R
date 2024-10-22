# Load in packages
library(tidyverse)
library(ggplot2)

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

# Let's see how these percentages change based on some situational factors
# First let's look at game context
# Change base indicators to dummy vars
pitch_df <- pitch_df %>%
  mutate(ON_1B = if_else(!is.na(ON_1B), 1, 0),
         ON_2B = if_else(!is.na(ON_2B), 1, 0),
         ON_3B = if_else(!is.na(ON_3B), 1, 0))

# Group by situational factors and get percentages
situational_percentages <- pitch_df %>%
  group_by(OUTS_WHEN_UP, ON_1B, ON_2B, ON_3B) %>%
  summarize(fastball_percentage = mean(fastball),
            off_speed_percentage = mean(off_speed),
            breaking_ball_percentage = mean(breaking_ball)) %>%
  ungroup()

# Create a new column to describe base runner situation
situational_percentages <- situational_percentages %>%
  mutate(Base_Runners = case_when(
    ON_1B == 1 & ON_2B == 1 & ON_3B == 1 ~ "Loaded",
    ON_1B == 1 & ON_2B == 1 & ON_3B == 0 ~ "1st & 2nd",
    ON_1B == 1 & ON_2B == 0 & ON_3B == 1 ~ "1st & 3rd",
    ON_1B == 0 & ON_2B == 1 & ON_3B == 1 ~ "2nd & 3rd",
    ON_1B == 1 & ON_2B == 0 & ON_3B == 0 ~ "1st",
    ON_1B == 0 & ON_2B == 1 & ON_3B == 0 ~ "2nd",
    ON_1B == 0 & ON_2B == 0 & ON_3B == 1 ~ "3rd",
    ON_1B == 0 & ON_2B == 0 & ON_3B == 0 ~ "Empty",
    TRUE ~ "Other"  # Catch any unexpected cases
  ))

# Set the order of Base_Runners as a factor with the desired levels
situational_percentages$Base_Runners <- factor(situational_percentages$Base_Runners, 
                                               levels = c("Empty", "1st", "2nd", "3rd", "1st & 2nd", "1st & 3rd", "2nd & 3rd", "Loaded"))

# Create a heatmap
ggplot(situational_percentages, aes(x = Base_Runners, y = factor(OUTS_WHEN_UP), fill = fastball_percentage)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Fastball Percentage by Outs and Base Runner Situation",
       x = "Base Runner Situation", y = "Outs", fill = "Fastball %") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),  
        plot.subtitle = element_text(hjust = 0.5, size = 10), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 14))

# Save the graph
ggsave("fb-pct-game-context.png", width = 14, height = 10, dpi = "retina")

# Now, let's look at how the count affects fastball percentages
# Group by count and get percentages
count_percentages <- pitch_df %>%
  group_by(BALLS, STRIKES) %>%
  summarize(fastball_percentage = mean(fastball),
            off_speed_percentage = mean(off_speed),
            breaking_ball_percentage = mean(breaking_ball),
            pitches = n()) %>%
  ungroup() %>%
  # Filter out data errors
  filter(pitches >= 5)

# Create a heatmap
ggplot(count_percentages, aes(x = BALLS, y = factor(STRIKES), fill = fastball_percentage)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Fastball Percentage by Count",
       x = "Balls", y = "Strikes", fill = "Fastball %") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        plot.subtitle = element_text(hjust = 0.5, size = 10), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 14))

# Save the graph
ggsave("fb-pct-count.png", width = 14, height = 10, dpi = "retina")

filled_predictions_df <- read_csv("filled_predictions.csv")

library(baseballr)
library(ggplot2)
library(tidyverse)
library(rvest)
library(janitor)
library(readr)
library(ggrepel)

# Creating PA/162 column because it was not available on statcast
# PA/162 normalizes the sample size while keeping accurate proportions
statcast2025$PA.162 <- statcast2025$pa / statcast2025$b_game * 162

# Creating taining and test data for the model
set.seed(123)
index <- sample(1:nrow(statcast2025), size = 0.8 * nrow(statcast2025))
train_df <- statcast2025[index, ]
test_df <- statcast2025[-index, ]

model2025 <- lm(barrel ~ PA.162 + slg_percent + on_base_percent +
                  exit_velocity_avg + sweet_spot_percent + swing_percent, data=train_df)
summary(model2025)

# Using test data frame to test if model fits data well
test_df$predictions <- predict(model2025, newdata = test_df)
ggplot(test_df, aes(x = predictions, y = barrel,label = last_name..first_name)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_text_repel(size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted Barrels vs. Actual Barrels",
       x = "Model Prediction",
       y = "Actual Observation")
# Model fits data pretty well

# Sorting data in ascending number of barrels
statcast2026 <- statcast2026[order(statcast2026$barrel), ]
rownames(statcast2026) <- NULL

# Adding PA.162 and PA.G columns to the 2026 dataset
statcast2026$PA.162 <- statcast2026$pa / statcast2026$b_game * 162
statcast2026$PA.G <- statcast2026$pa / statcast2026$b_game
barrel_blunderers <- statcast2026 %>%
  filter(barrel < 1 & PA.G >= 2.5)

# Creating predicitons resulting from the model
barrel_blunderers$predictions <- predict(model2025, newdata = barrel_blunderers)

# Filtering for candidates with negative predicted barrels
best_barrel_blunderers <- barrel_blunderers %>%
  filter(predictions < 0)

# Graph of Predicted Barrels vs Actual Barrels for Barrel Blunderer Contenders
ggplot(best_barrel_blunderers, aes(x = predictions, y = barrel,label = last_name..first_name)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_text_repel(size = 3) +
  labs(title = "Predicted Barrels vs. Actual Barrels",
       x = "Model Prediction",
       y = "Actual Observation")

# Graph of Predicted Barrels vs Avg Exit Velo
ggplot(best_barrel_blunderers, aes(x = predictions, y = exit_velocity_avg,label = last_name..first_name)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_text_repel(size = 3) +
  labs(title = "Predicted Barrels vs. Avg Exit Velo",
       x = "Barrels Prediction",
       y = "Avg Exit Velo")

# Graph of Predicted barrels vs PA/G
ggplot(best_barrel_blunderers, aes(x = predictions, y = PA.G,label = last_name..first_name)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_text_repel(size = 3) +
  geom_abline(slope = 0, intercept = 3.1, color = "red", linetype = "dashed") +
  labs(title = "Predicted Barrels vs. PA/G",
       x = "Barrels Prediction",
       y = "PA/G")

# Reduced possible barrel blunderer winners to the top 13
barrel_blunderers_T13 <- barrel_blunderers %>%
  filter(predictions < -16)

# Manually inputted data from the last 14 days from Baseball Reference to the dataframe
barrel_blunderers_T13$PA.G.14G[barrel_blunderers_T13$last_name..first_name == 'Walls, Taylor'] <- 4.09
barrel_blunderers_T13 <- barrel_blunderers_T13 %>%
  mutate(PA.G.14G = if_else(last_name..first_name == "Kiner-Falefa, Isiah", 2.57, PA.G.14G))

# Graph of Predicted Barrels vs. PA/G and PA/G over the last 14 days
# Required to match labels to correct points
plot_data <- barrel_blunderers_T13 %>%
  pivot_longer(cols = c(PA.G, PA.G.14G), 
               names_to = "PA_Type", 
               values_to = "PA_Value")

ggplot(plot_data, aes(x = predictions, y = PA_Value, color = PA_Type)) +
  geom_line(aes(group = last_name..first_name), color = "grey90") + # Connects the two points
  geom_point(alpha = 0.7) +
  geom_text_repel(aes(label = last_name..first_name), size = 3) +
  geom_hline(yintercept = 3.1, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("PA.G" = "darkblue", "PA.G.14G" = "red"),
                     labels = c("Season PA/G", "Last 14 Days PA/G")) +
  labs(title = "Predicted Barrels vs. PA/G Qualification Trends",
       x = "Model Prediction (Barrels)",
       y = "Plate Appearances per Game",
       color = "Metric") +
  theme_minimal()

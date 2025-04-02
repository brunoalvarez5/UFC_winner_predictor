
# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)

# Load datasets
fight_data <- read.csv('ufc_fight_data.csv')
fighter_data <- read.csv('ufc_fighter_data.csv')
fight_stat_data <- read.csv('ufc_fight_stat_data.csv')

# Merge datasets
fight_stats <- fight_stat_data %>% 
  pivot_wider(names_from = fighter_id, 
              values_from = c(knockdowns, total_strikes_att, total_strikes_succ, 
                              sig_strikes_att, sig_strikes_succ, takedown_att, 
                              takedown_succ, submission_att, reversals, ctrl_time))

merged_data <- fight_data %>% 
  left_join(fight_stats, by = "fight_id") %>% 
  left_join(fighter_data, by = c("f_1" = "fighter_id"), suffix = c("_f1", "_f2"))

# Feature Engineering
merged_data <- merged_data %>% 
  mutate(
    height_diff = fighter_height_cm_f1 - fighter_height_cm_f2,
    reach_diff = fighter_reach_cm_f1 - fighter_reach_cm_f2,
    weight_diff = fighter_weight_lbs_f1 - fighter_weight_lbs_f2,
    age_diff = as.numeric(difftime(fighter_dob_f1, fighter_dob_f2, units = "days")) / 365,
    sig_strike_acc_diff = (sig_strikes_succ_f1 / sig_strikes_att_f1) - (sig_strikes_succ_f2 / sig_strikes_att_f2)
  ) %>% 
  mutate(winner_flag = ifelse(winner == f_1, 1, 0)) %>% 
  drop_na()

# Train-Test Split
set.seed(123)
train_index <- createDataPartition(merged_data$winner_flag, p = 0.8, list = FALSE)
train_data <- merged_data[train_index, ]
test_data <- merged_data[-train_index, ]

# Train a Random Forest Model
rf_model <- randomForest(winner_flag ~ height_diff + reach_diff + weight_diff + age_diff + sig_strike_acc_diff, 
                         data = train_data, importance = TRUE)

# Evaluate the Model
predictions <- predict(rf_model, test_data, type = "response")
confusionMatrix(as.factor(predictions), as.factor(test_data$winner_flag))

# Save the script output for analysis
write.csv(importance(rf_model), "variable_importance.csv")

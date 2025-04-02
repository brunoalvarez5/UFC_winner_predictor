# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)

# Load the dataset
file_path <- "ufc-fighters-statistics.csv"
ufc_data <- read.csv(file_path)

# Data Preprocessing
## Combine wins, losses, and draws into a target variable: "outcome"
ufc_data$outcome <- ifelse(ufc_data$wins > ufc_data$losses & ufc_data$wins > ufc_data$draws, "win",
                           ifelse(ufc_data$losses > ufc_data$wins & ufc_data$losses > ufc_data$draws, "lose", "draw"))
ufc_data$outcome <- as.factor(ufc_data$outcome)

## Select relevant features and handle missing values
features <- c("height_cm", "weight_in_kg", "reach_in_cm", 
              "significant_strikes_landed_per_minute", "significant_striking_accuracy",
              "significant_strikes_absorbed_per_minute", "significant_strike_defence",
              "average_takedowns_landed_per_15_minutes", "takedown_accuracy", "takedown_defense",
              "average_submissions_attempted_per_15_minutes", "outcome")
ufc_data <- ufc_data %>% select(all_of(features))

# Remove rows with missing values
ufc_data <- na.omit(ufc_data)


# Train-Test Split
set.seed(123)
train_index <- createDataPartition(ufc_data$outcome, p = 0.8, list = FALSE)
train_data <- ufc_data[train_index, ]
test_data <- ufc_data[-train_index, ]

# Train a Random Forest Model
rf_model <- randomForest(outcome ~ ., data = train_data, importance = TRUE, ntree = 100)

# Model Evaluation
rf_predictions <- predict(rf_model, newdata = test_data)
confusionMatrix(rf_predictions, test_data$outcome)

# Variable Importance
importance(rf_model)
varImpPlot(rf_model)

# Save the model for future use
saveRDS(rf_model, "ufc_outcome_model.rds")

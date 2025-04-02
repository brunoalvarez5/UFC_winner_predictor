# Load the saved model
rf_model <- readRDS("ufc_outcome_model.rds")

# Create a new fighter's data as a data frame
new_fighter <- data.frame(
  height_cm = 180,
  weight_in_kg = 70,
  reach_in_cm = 185,
  significant_strikes_landed_per_minute = 3.5,
  significant_striking_accuracy = 50,
  significant_strikes_absorbed_per_minute = 2.5,
  significant_strike_defence = 60,
  average_takedowns_landed_per_15_minutes = 1.2,
  takedown_accuracy = 40,
  takedown_defense = 65,
  average_submissions_attempted_per_15_minutes = 0.3
)

# Predict the outcome
predicted_outcome <- predict(rf_model, newdata = new_fighter)

print(predicted_outcome)

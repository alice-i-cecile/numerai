# Load current model
my_model <- load("./Models/my_model.RData")

# Generate predictions
tournament_data <- read.csv("./Data/numerai_tournament_data.csv")
predictions <- predict(my_model, newsdata = tournament_data)

# Saving outputs
write.csv(predictions, "./Predictions/my_predictions.csv")
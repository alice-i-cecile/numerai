# Libraries ####
library(ggplot2)

# Model fitting scripts ####
source("./Scripts/Models/fit_glmnet.R")
source("./Scripts/Models/fit_lm.R")
source("./Scripts/Models/fit_logistic.R")
source("./Scripts/Models/fit_null.R")
#source("./Scripts/Models/fit_rf.R")

# Setup ####

# Load in the data set
my_data <- read.csv("./Data/numerai_training_data.csv")

# Split into training and test partitions
set.seed(42)
train_split <- createDataPartition(y = my_data$target, times = 1, p = 0.7)

train_data <- my_data[train_split[[1]],]
test_data <- my_data[-train_split[[1]],]

# Model fitting wrapper ####
fit_model <- function(method="lm", ...){
    model <- switch(method,
                    "glmnet" = fit_glmnet(train_data, ...),
                    "lm" = fit_lm(train_data, ...),
                    "logistic" = fit_logistic(train_data, ...),
                    "null" = fit_null(train_data, ...),
                    "rf" = fit_rf(train_data, ...))
    
    return(model)
}

# Fitting the model selected
#my_model <- fit_model(method = "lm")
#my_model <- fit_model(method = "logistic")
my_model <- fit_model(method = "null")


# Test data evaluation ####
train_predictions <- predict(my_model, type = "response")
test_predictions <- predict(my_model, newdata = test_data, type = "response")

# Plotting actual vs. predicted
predictions_df <- rbind(data.frame(Split = "Training", Predicted = train_predictions, Actual = train_data$target),
                        data.frame(Split = "Test", Predicted = test_predictions, Actual = test_data$target)       
                  )

ggplot(predictions_df, aes(x = as.factor(Actual), y = Predicted)) + geom_violin(fill = "grey") + facet_grid(Split~.) + theme_bw()

# Performance metric
binary_logloss <- function(actual, predicted){
    epsilon <- 1e-15
    predicted <- max(epsilon, predicted)
    predicted <- min(1 - epsilon, predicted)
    ll <- sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))
    ll <- ll * -1/length(actual)
    return(ll)
}

train_score <- binary_logloss(train_data$target, train_predictions)
test_score <- binary_logloss(test_data$target, test_predictions)

print(paste("Training:", train_score))
print(paste("Test:", test_score))

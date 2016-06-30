# Libraries ####
library(ggplot2)
library(caret)

# Model fitting scripts ####
source("./Scripts/Models/fit_gam.R")
source("./Scripts/Models/fit_glmnet.R")
source("./Scripts/Models/fit_lm.R")
source("./Scripts/Models/fit_logistic.R")
source("./Scripts/Models/fit_null.R")
source("./Scripts/Models/fit_rf.R")

# Setup ####

# Load in the data set
my_data <- read.csv("./Data/numerai_training_data.csv")

my_data <- my_data[1:1E3,]

# Split into training and test partitions
set.seed(42)
train_split <- createDataPartition(y = my_data$target, times = 1, p = 0.7)

train_data <- my_data[train_split[[1]],]
test_data <- my_data[-train_split[[1]],]

# Model fitting wrapper ####
fit_model <- function(method="lm", ...){
    model <- switch(method,
                    "gam" = fit_gam(train_data, ...),
                    "glmnet" = fit_glmnet(train_data, ...),
                    "lm" = fit_lm(train_data, ...),
                    "logistic" = fit_logistic(train_data, ...),
                    "null" = fit_null(train_data, ...),
                    "rf" = fit_rf(train_data, ...))
    
    return(model)
}

# Fitting the model selected
my_method <- "null"

my_model <- fit_model(method = my_method)

# Test data evaluation ####

if (my_method %in% c("logistic", "gam")) {
    train_predictions <- predict(my_model, type = "response")
    test_predictions <- predict(my_model, newdata = test_data, type = "response")
} else if (my_method %in% c("glmnet")) {
    train_predictions <- predict(my_model, type = "prob")
    test_predictions <- predict(my_model, newdata = test_data, type = "prob")
} else if (my_method %in% c("rf")) {
    train_predictions <- predict(my_model, type = "prob")[["1"]]
    test_predictions <- predict(my_model, newdata = test_data, type = "prob")[["1"]]
} else {
    train_predictions <- predict(my_model)
    test_predictions <- predict(my_model, newdata = test_data)
}

# Plotting actual vs. predicted
predictions_df <- rbind(data.frame(Split = "Training", Predicted = train_predictions, Actual = train_data$target),
                        data.frame(Split = "Test", Predicted = test_predictions, Actual = test_data$target)       
                  )

ggplot(predictions_df, aes(x = as.factor(Actual), y = Predicted)) + geom_violin(fill = "grey") + facet_grid(Split~.) + theme_bw() + xlab("Actual")

# Performance metric
binary_logloss <- function(actual, predicted){
    eps <- 1e-16
    predicted <- pmin(pmax(predicted, eps), 1 - eps)
    ll <- -(sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
    return(ll)
}

train_score <- binary_logloss(train_data$target, train_predictions)
test_score <- binary_logloss(test_data$target, test_predictions)

print(paste("Training:", train_score))
print(paste("Test:", test_score))

# Saving the model ####
#save(my_model, paste0("./Models/", my_method, "_model.RData"))

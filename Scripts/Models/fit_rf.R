# Libraries ####
library(caret)
library(randomForest)

# Fitting funciton ####
fit_rf <- function(input_data){
    
    y <- input_data$target
    x <- subset(input_data, select = -c(target))
    
    model <- train(y = as.factor(y),
                   x = x,
                   method = "rf")
    
    return(model)
}
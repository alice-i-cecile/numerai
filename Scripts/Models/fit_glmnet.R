# Libraries ####
library(caret)
library(glmnet)

# Fitting funciton ####
fit_glmnet <- function(input_data){
    #lambda.seq <- exp(seq(-2, 2, by = 0.1))
    lambda.seq <- exp(seq(-3, -1, by = 0.1))
    #alpha.seq <- seq(0, 1, by = 0.1)
    alpha.seq <- 0
    
    y <- input_data$target
    x <- subset(input_data, select = -c(target))
    
    model <- train(y = as.factor(y),
                   x = x,
                   method = "glmnet",
                   family = "binomial",
                   tuneGrid = expand.grid(alpha = alpha.seq, lambda = lambda.seq))
    
    return(model)
}
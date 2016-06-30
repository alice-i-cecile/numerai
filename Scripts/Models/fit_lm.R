fit_lm <- function(input_data){
    model <- lm(formula = target~., data = input_data)
    return(model)
}
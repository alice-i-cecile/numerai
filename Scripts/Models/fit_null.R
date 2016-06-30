fit_null <- function(input_data){
    model <- lm(formula = target~1, data = input_data)
    return(model)
}
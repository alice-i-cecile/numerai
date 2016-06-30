fit_logistic <- function(input_data){
    model <- glm(formula = target~., data = input_data, family = binomial(link = "logit"))
    return(model)
}
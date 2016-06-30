library(mgcv)

fit_gam <- function(input_data){
    terms <- setdiff(names(input_data), "target")
    smooth_terms <- paste0("s(", terms,")")
    LHS <- "target~"
    RHS <- Reduce(function(a,b){paste(a,b, sep = "+")}, smooth_terms)
    my_formula <- formula(paste0(LHS, RHS))
    model <- gam(formula = my_formula, data = input_data, family = binomial(link = "logit"))
    return(model)
}
# Performance metrics ####
LogLossBinary <- function(actual, predicted, eps = 1e-15) {
    predicted <- pmin(pmax(predicted, eps), 1 - eps)
    ll <- -(sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
    return(ll)
}

# Prec <- function(actual, predicted){
#     Prec <- actual/actual + truepos
#     return(Prec)
# }
# Recall <- function(actual,predicted){
#     Recall <-actual/actual+ falsepos
#     return(Recall)
# }
# 
# 
# Fscore <- 
# 
# kappa <- function

RMSE <- function(actual, predicted){
    rmse <- sqrt(mean((actual - predicted) ^ 2))
    return(rmse)
}

# Simulation ####

n <- 100
samples <- 1000

random_samples <- function(x){
    foo <- rbinom(n, 1, 0.5)
    bar <- runif(n, 0, 1)
    out <- data.frame(ll = LogLossBinary(foo, bar),
                      cor = cor(foo, bar),
                      rho = cor(foo, bar, method = "spearman"),
                      #kappa = kappa(foo, bar),
                      #Fscore = Fscore(foo, bar),
                      RMSE = RMSE(foo, bar))
    return(out)
}

results <- Reduce(rbind, lapply(1:samples, random_samples))


plot(results)
cor(results)


library(BASS)

my_fit <- function(X, y){
  bass(X, y, g1=0.001, g2=0.001)
}

my_pred <- function(obj, Xt){
  res <- predict(obj, Xt)
  return(res)
}

res <- run_sim_study_data(my_fit, my_pred,
                   dnames=c("pbx9501_gold", "strontium_plume_p104"),
                   folds=3)




library(BASS)

my_fit <- function(X, y){
  bass(X, y, g1=0.001, g2=0.001)
}

my_pred <- function(obj, Xt, conf_level=0.95){
  alpha <- 1 - conf_level
  preds <- predict(obj, Xt)
  yhat <- apply(preds, 2, mean)
  sd <- sqrt(apply(preds, 2, var) + mean(obj$s2))
  res <- cbind(yhat, yhat-2*sd, yhat + 2*sd)
  return(res)
}

run_sim_study_data(my_fit, my_pred,
                   dnames=c("pbx9501_gold", "strontium_plume_p104"),
                   folds=3)


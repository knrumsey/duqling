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

data <- run_sim_study(my_fit, my_pred,
                      fnames = c("dms_additive", "dms_simple"),
                      n_train = 100,
                      NSR = 0,
                      design_type="LHS",
                      replications=6, mc_cores=3)

my_fit_f1 <- function(X, y){
  bass(X, y, g1=0.001, g2=0.001, nmcmc=1000, nburn=100)
}

my_pred_f1 <- function(obj, Xt, conf_level=0.95){
  alpha <- 1 - conf_level
  preds <- predict(obj, Xt)
  yhat <- apply(preds, 2, mean)
  sd <- sqrt(apply(preds, 2, var) + mean(obj$s2))
  res <- cbind(yhat, yhat-2*sd, yhat + 2*sd)
  return(res)
}
my_fit <- list(my_fit_f1, my_fit_f1)
my_pred <- list(my_pred_f1, my_pred_f1)

data <- run_sim_study(my_fit, my_pred,
                      fnames = c("dms_additive", "dms_simple"),
                      n_train = 100,
                      NSR = 0,
                      design_type="LHS",
                      method_names=c("BASS", "BASS2"),
                      replications=6, mc_cores=3)




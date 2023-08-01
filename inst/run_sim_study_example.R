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

tictoc::tic()
data <- run_sim_study(my_fit, my_pred,
                      fnames = c("dms_additive", "dms_simple", "dms_radial", "dms_harmonic", "dms_additive",
                                 "squiggle", "twin_galaxies", "micwicz", "detpep_curve", "const_fn3",
                                 "piston", "circuit", "borehole", "robot", "wingweight", "detpep8", "friedman",
                                 "friedman10", "const_fn", "const_fn15", "welch20"),
                      n_train = c(100, 500),
                      NSR = c(0, 1/5),
                      design_type="LHS",
                      replications=5)
tictoc::toc()




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






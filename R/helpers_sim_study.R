lookup_sigma <- function(fname){
  sigma_lookup <- list(
    banana=3.710725e+02,
    borehole_low_fidelity=3.635870e+01,
    borehole=4.555280e+01,
    cantilever_D=3.286210e-01,
    cantilever_S=7.842194e+03,
    circuit=1.141898e+00,
    const_fn=0.000000e+00   + 1e-7, # Don't return 0 here
    const_fn15=0.000000e+00 + 1e-7, # Don't return 0 here
    const_fn3=0.000000e+00  + 1e-7, # Don't return 0 here
    crater=1.415364e+00,
    cube3_rotate=7.037742e-01,
    cube3=5.572555e-01,
    cube5=2.442026e+00,
    detpep_curve=3.610622e+01,
    detpep8=3.649000e+01,
    dms_additive=9.873234e-01,
    dms_complicated=1.002189e+00,
    dms_harmonic=9.218845e-01,
    dms_radial=9.845250e-01,
    dms_simple=9.619880e-01,
    ebola=4.904622e-01,
    forrester1_low_fidelity=6.083980e+00,
    forrester1=9.580872e+00,
    foursquare=3.829575e-01,
    friedman=4.826403e+00,
    friedman10=4.832760e+00,
    friedman20=4.831938e+00,
    gamma_mix=7.687958e-02,
    Gfunction=9.343063e-01,
    Gfunction12=9.162019e-01,
    Gfunction18=1.968464e+00,
    Gfunction6=8.893865e-01,
    grlee1=1.305325e+00,
    grlee2=7.703807e-02,
    grlee6=7.158600e-01,
    ignition=2.479687e+00,
    ishigami=3.716563e+00,
    lim_non_polynomial=1.951328e+00,
    lim_polynomial=1.634255e+00,
    multivalley=3.521684e-01,
    oo15=1.523091e+01,
    park4_low_fidelity=5.047334e+00,
    park4=4.749824e+00,
    permdb=4.478022e-02,
    piston=1.381331e-01,
    pollutant_uni=8.304634e-01,
    rabbits=2.857249e-01,
    ripples=7.754145e-02,
    robot=5.236244e-01,
    sharkfin=2.985767e-01,
    short_column=4.063448e+00,
    simple_poly=4.775166e-01,
    squiggle=5.377997e-01,
    star2=1.144571e+00,
    steel_column=1.064768e+02,
    sulfur=3.055872e+01,
    twin_galaxies=1.024249e+00,
    welch20=2.093332e+00,
    wingweight=4.807968e+01,
    onehundred=5.733751e+00,
    # STARTING DATA SETS HERE
    plate_deformation=1.449489e-02,
    wind_speed=1.027572e+00,
    strontium_plume_p4b=2.808191e-01,
    strontium_plume_p104=1.469877e-01,
    spectra1=1.000000e+00,
    spectra2=1.000000e+00,
    pbx9501_gold=6.320689e-03,
    pbx9501_ss304=6.826252e-03,
    pbx9501_nickel=6.838146e-03,
    pbx9501_uranium=6.980380e-03,
    ptw1=6.597825e-03,
    discoflux_flyer=3.579125e-02,
    taylor_foot=1.259407e-01,
    taylor_length=1.926734e-01,
    jc=4.089584e-03,
    flyerPTW1=1.589439e-02,
    flyerPTW2=2.508928e-03,
    flyer_plate104=6.348922e+01,
    diffusion_1D=2.766821e-02,
    "fair_climate_ssp1-2.6_year2200"=4.126652e-01,
    "fair_climate_ssp2-4.5_year2200"=7.037122e-01,
    "fair_climate_ssp3-7.0_year2200"=1.202978e+00,
    taylor_cylinder1=2.168441e-02,
    stochastic_sir=2.009268e+03,
    diffusion_2D=1.968937e-01,
    acme_climate=2.609752e+02,
    ptw2=7.001573e-03,
    SLOSH_low=7.484742e-01,
    SLOSH_mid=9.763949e-01,
    SLOSH_high=1.344907e+00,
    Z_machine_max_vel1=1.082886e+02,
    Z_machine_max_vel2=7.478024e+01,
    Z_machine_max_vel_all=5.396926e+02,
    e3sm_mnar=1.513867e+01,
    nuclear_data=1.068442e-02,
    e3sm_mcar=1.492739e+01,
    icf1=9.876422e-02,
    icf2=1.964187e-01,
    diablo_canyon_plume=4.958526e-01,
    okc_plume=6.473348e-01
  )

  return(sigma_lookup[[fname]])
}


#' Human-friendly labels for derived metrics
#'
#' Converts internal column names like "CRPS_rank" or "CRPS_rel_log"
#' into nicer labels for plotting.
#'
#' @param metric Character; column name.
#'
#' @return A character label.
#' @export
metric_label <- function(metric) {
  if (metric == "time") {
    return("Time")
  }
  if (metric == "time_predict") {
    return("Prediction time")
  }
  if (metric == "time_fit") {
    return("Training time")
  }

  if (grepl("_rank$", metric)) {
    base <- sub("_rank$", "", metric)
    return(paste(base, "Rank"))
  }
  if (grepl("_auc$", metric)) {
    base <- sub("_auc$", "", metric)
    return(paste("AUC of", base, "Rank"))
  }
  if (grepl("_rel_log$", metric)) {
    base <- sub("_rel_log$", "", metric)
    return(paste("log Relative", base))
  }
  if (grepl("_rel$", metric)) {
    base <- sub("_rel$", "", metric)
    return(paste("Relative", base))
  }
  if (grepl("_norm$", metric)) {
    base <- sub("_norm$", "", metric)
    return(paste(base, "(z-score)"))
  }
  return(metric)
}

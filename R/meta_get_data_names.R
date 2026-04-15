#' @name get_sim_data
#' @rdname get_sim_data
#'
#' @title Sim Study Datset Lists
#'
NULL

#' @rdname get_sim_data
#' @export
get_sim_data_full <- function(){
  data_quack(raw=FALSE, input_cat_dim = 0)$dname
}

#' @rdname get_sim_data
#' @export
get_sim_data_standard <- function(){
  c("e3sm_mcar", "e3sm_mnar",
    "stochastic_sir",
    "SLOSH_low", "SLOSH_high",
    "pbx9501_gold", "pbx9501_uranium",
    "Z_machine_max_vel1",
    "Z_machine_max_vel_all",
    "flyer_plate104",
    "strontium_plume_p4b",
    "fair_climate_ssp1-2.6_year2200")
}

#' @rdname get_sim_data
#' @export
get_sim_data_tiny <- function(){
  c("strontium_plume_p4b", "pbx9501_gold")
}

#' @rdname get_sim_data
#' @export
get_sim_data_medium <- function(){
  c("strontium_plume_p4b", "strontium_plume_p104", "stochastic_sir",
    "pbx9501_gold", "pbx9501_nickel")
}

#' @rdname get_sim_data
#' @export
get_sim_data_2d <- function(){
  data_quack(raw=FALSE, input_cat_dim = 0, input_dim = 2)$dname
}

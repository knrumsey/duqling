#' @name get_funcs
#' @rdname get_funcs
#'
#' @title Sim Study Function Lists
#'
NULL

#' @rdname get_funcs
#' @export
get_sim_functions_full <- function(){
  quack(input_cat = FALSE, response_type="uni", stochastic="n")$fname
}
#' @rdname get_funcs
#' @export
get_sim_functions_2d <- function(){
  quack(input_cat = FALSE, response_type="uni", stochastic="n", input_dims=2)$fname
}
#' @rdname get_funcs
#' @export
get_sim_functions_medium <- function(){
  c("dms_additive", "dms_simple", "dms_radial", "dms_harmonic", "dms_complicated",
    "squiggle", "twin_galaxies", "micwicz", "foursquare", "pollutant_uni",
    "piston", "circuit", "borehole", "robot", "wingweight", "detpep8", "friedman",
    "friedman10", "const_fn15", "welch20")
}
#' @rdname get_funcs
#' @export
get_sim_functions_short <- function(){
  c("dms_simple", "dms_complicated", "twin_galaxies",
    "borehole", "robot", "piston", "circuit", "const_fn3",
    "wingweight", "friedman10")
}
#' @rdname get_funcs
#' @export
get_sim_functions_tiny <- function(){
  c("dms_complicated", "borehole", "piston", "friedman10")
}

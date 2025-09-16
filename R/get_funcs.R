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
  quack(input_cat = FALSE, response_type="uni", stochastic="n", input_dim=2)$fname
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


#' @rdname get_funcs
#' @export
get_paper_funcs <- function(){
  c("const_fn",
    "forrester1",
    "forrester1_low_fidelity",
    "grlee1",
    "banana",
    "dms_additive",
    "dms_complicated",
    "dms_harmonic",
    "dms_radial",
    "dms_simple",
    "foursquare",
    "grlee2",
    "lim_non_polynomial",
    "lim_polynomial",
    "multivalley",
    "ripples",
    "simple_poly",
    "squiggle",
    "star2",
    "twin_galaxies",
    "const_fn3",
    "cube3",
    "cube3_rotate",
    "detpep_curve",
    "Gfunction",
    "ishigami",
    "rabbits",
    "sharkfin",
    "park4",
    "park4_low_fidelity",
    "pollutant_uni",
    "cube5",
    "friedman",
    "short_column",
    "cantilever_D",
    "cantilever_S",
    "circuit",
    "Gfunction6",
    "grlee6",
    "crater",
    "gamma_mix",
    "piston",
    "borehole",
    "borehole_low_fidelity",
    "detpep8",
    "ebola",
    "robot",
    "steel_column",
    "sulfur",
    "friedman10",
    "ignition",
    "wingweight",
    "Gfunction12",
    "const_fn15",
    "oo15",
    "permdb",
    "Gfunction18",
    "friedman20",
    "welch20",
    "onehundred")
}




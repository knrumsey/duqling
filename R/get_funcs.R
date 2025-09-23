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
get_sim_functions_tiny <- function(){
  # Classic functions from UQ literature
  c("ishigami", "piston", "borehole", "friedman10", "Gfunction18")
}

#' @rdname get_funcs
#' @export
get_sim_functions_short <- function(){
  c("ishigami", "piston", "borehole", "friedman10", "Gfunction18",
    "pollutant_uni", "cube5", "crater", "ebola", "ignition") # add some diversity of function behavior
}

#' @rdname get_funcs
#' @export
get_sim_functions_medium <- function(){
  c("ishigami", "pollutant_uni", "cube5", "piston", "crater",
    "borehole", "ebola", "ignition", "friedman10", "Gfunction18",
    "grlee1", "foursquare", "squiggle", "ripples", "multivalley",   # Add some diversity in 1d and 2d test functions
    "cube3_rotate", "cantilever_D", "Gfunction6", "oo15", "welch20" # Add some diversity in medium to high dimension
    )
}

#' @rdname get_funcs
#' @export
get_sim_functions_dms <- function(){
  c("dms_simple", "dms_additive", "dms_complicated", "dms_radial", "dms_harmonic")
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




#' @name constant
#' @rdname constant
#'
#' @title Constant Functions
#'
#' @param x Inputs of any dimension. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @details Always returns 0
NULL

#' @rdname constant
#' @export
const_fn <- function(x, scale01=TRUE){
  return(0)
}


#' @rdname constant
#' @export
const_fn3 <- function(x, scale01=TRUE){
  const_fn(x)
}

#' @rdname constant
#' @export
const_fn15 <- function(x, scale01=TRUE){
  const_fn(x)
}

quackquack_const_fn <- function(){
  out <- list(input_dim=1)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(0,
              1)
  rownames(RR) <- c("x")
  out$input_range <- RR

  return(out)
}

quackquack_const_fn3 <- function(){
  out <- list(input_dim=3)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(0,0,0),c(1,1,1))
  rownames(RR) <- c("x1", "x2", "x3")
  out$input_range <- RR

  return(out)
}

quackquack_const_fn15 <- function(){
  out <- list(input_dim=15)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(rep(0, 15),
              rep(1, 15))
  rownames(RR) <- unlist(lapply(1:15,
                                function(zz) paste0("x", zz)))
  out$input_range <- RR

  return(out)
}

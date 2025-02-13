#' @title Short Column
#'
#' @description
#' Dimension 5.
#'
#' @param x Inputs of dimension (at least) 5. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval.
#' @details The Short Column function models a short column with uncertain material properties and dimensions sbject to uncertain loads. Y is the yield stress, M is the bending moment, P is the axial force, b is the width of the cross section, and h is the depth of the cross section. Note that sometimes M and P are given a correlated prior (correlation coefficient = 0.5).
#' For more details on the Cantilever Beam, see \href{https://www.sfu.ca/~ssurjano/shortcol.html}{the VLSE}.
#' @references
#' Eldred, M. S., et al. "Investigation of reliability method formulations in DAKOTA/UQ." Structure and Infrastructure Engineering 3.3 (2007): 199-213.
#'
#' Kuschel, Norbert, and Rüdiger Rackwitz. "Two basic problems in reliability-based structural optimization." Mathematical Methods of Operations Research 46 (1997): 309-333.
#' @rdname short_column
#' @examples
#' X <- lhs::randomLHS(50, 7)
#' y <- apply(X, 1, short_column, scale01=TRUE)
#' @export
short_column <- function(x, scale01=TRUE){
  if(scale01){
    RR <- rbind(c(4.8, 5.1), c(400, 3600), c(100, 900), c(3,7), c(10, 20))
    x[1:5] <- x[1:5]*(RR[,2] - RR[,1]) + RR[,1]
  }

  Y <- x[1]
  M <- x[2]
  P <- x[3]
  b <- x[4]
  h <- x[5]
  res <- 1 - 4*M/(b*h^2*Y) - P^2/(b^2*h^2*Y^2)
  return(res)
}



quackquack_short_column <- function(){
  out <- list(input_dim=5)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  RR <- rbind(c(4.8, 5.1), c(400, 3600), c(100, 900), c(3,7), c(10, 20))

  rownames(RR) <- c("Y", "M", "P", "b", "h")
  out$input_range <- RR
  return(out)
}

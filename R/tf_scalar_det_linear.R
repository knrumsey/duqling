#' Linear and Quadratic Test Functions
#'
#' Simple linear and quadratic benchmark functions.
#'
#' @param x Inputs of dimension at least p.
#' @param scale01 Are inputs given on (0, 1) scale?
#' @param a Vector of coefficients for linear terms.
#' @param b Vector of coefficients for quadratic terms.
#' @param c Vector of coefficients for pairwise interaction terms.
#' @return Scalar output.
#' @details Simple linear or quadratic models.
#'
#' @export
#' @examples
#' fname <- "linear21"
#' n <- 10
#' p <- quack(fname, verbose=FALSE)$input_dim
#' x <- matrix(runif(n*p), nrow=n)
#' y <- eval_duq(fname, x)
linear <- function(x, scale01=TRUE,
                   a = NULL){
  if(is.null(a)){
    a <- c(-0.6, 0.3, 0.05, 1.1)
  }
  p <- length(a)
  sum(a * x[1:p])
}

#' @rdname linear
#' @export
linear21 <- function(x, scale01=TRUE){
  a <- -10:10
  sum(a * x[1:21])
}

#' @rdname linear
#' @export
linear21_s10 <- function(x, scale01=TRUE){
  a <- -10:10
  a[c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 21)] <- 0
  sum(a * x[1:21])
}

#' @rdname linear
#' @export
linear21_s1 <- function(x, scale01=TRUE){
  a <- c(10, rep(0, 20))
  sum(a * x[1:21])
}

#' @rdname linear
#' @export
quad <- function(x, scale01=TRUE, a = NULL, b = NULL, c = NULL){
  if(is.null(a)){
    a <- c(-1.2, 0.6, 0.05, 1.1)
  }
  if(is.null(b)){
    b <- c(0, 0, 1, -1)
  }
  if(is.null(c)){
    c <- c(0, 0, -0.5, 0, 0, 2)
  }

  p <- max(length(a), length(b))

  p_a <- p - length(a)
  a <- c(a, rep(0, p_a))

  p_b <- p - length(b)
  b <- c(b, rep(0, p_b))

  p_int <- choose(p, 2)
  idx <- combn(1:p, 2)
  p_c <- length(c) - p_int
  if(p_c > 0){
    c <- c[1:p_int]
  }else{
    c <- c(c, rep(0, p_c))
  }

  res <- sum(a*x + b*x^2) + sum(c*x[idx[1,]]*x[idx[2,]])
}

#' @rdname linear
#' @export
quad4 <- function(x, scale01=TRUE){
  quad(x, scale01,
       a = c(-1.2, 0.6, 0.05, 1.1),
       b = c(0, 0, 1, -1),
       c = c(0, 0, -0.5, 0, 0, 2)
       )
}


# Write quackquack functions
#' @export
quackquack_linear <- function(){
  out <- list(input_dim = 4)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  RR <- cbind(rep(0, 4), rep(1, 4))
  rownames(RR) <- paste0("x", 1:4)
  out$input_range <- RR
  out
}

#' @export
quackquack_linear21 <- function(){
  out <- list(input_dim = 21)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  RR <- cbind(rep(0, 21), rep(1, 21))
  rownames(RR) <- paste0("x", 1:21)
  out$input_range <- RR
  out
}

#' @export
quackquack_linear21_s10 <- quackquack_linear21

#' @export
quackquack_linear21_s1 <- quackquack_linear21

#' @export
quackquack_quad <- function(){
  out <- list(input_dim = 4)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  RR <- cbind(rep(0, 4), rep(1, 4))
  rownames(RR) <- paste0("x", 1:4)
  out$input_range <- RR
  out
}

#' @export
quackquack_quad4 <- quackquack_quad

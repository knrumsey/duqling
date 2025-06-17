#' Ripples Function
#'
#' Dimensions 2. The ripples function was used by Rumsey et al (2022) to construct small scale structures for synthetic data mimicking the E3SM climate model.
#'
#' @param x Inputs of dimension (at least) 8. See below for details.
#' @param scale01 (No effect here). When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param fixed_coeff logical. Should coefficients be fixed at values from Rumsey et al (2022)? Ignored when \code{input_dims != 2}.
#' @param input_dims desired input dimension.
#' @param seed Optional seed for coefficient generation.
#' @return Univariate response representing small scale structure
#' @details The number of inputs can be made arbitrary, but only by setting \code{fixed_coeff = FALSE}. When \code{fixed_coeff = TRUE}, the ripples function exactly matches the function from Rumsey et al (2022). Otherwise, the coefficients are randomly generated from a standard normal distribution. All inputs are expected between zero and one
#' @references
#' Rumsey et al reference needed (SPIE conference)
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(2*n), nrow=n)
#' y <- apply(x, 1, ripples)
ripples <- function(x, scale01=TRUE, fixed_coeff=TRUE, input_dims=2, seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  if(fixed_coeff == TRUE & input_dims != 2) warning("Cannot have fixed coefficients when input_dims != 2.")

  if(fixed_coeff){
    W <- cbind(c(-0.138, 0.348, 1.628, -1.452, 0.955),
               c(1.905, 1.520, -1.533, 0.185, -1.440))
  }
  if(!fixed_coeff | input_dims != 2){
    W <- matrix(stats::rnorm(5*input_dims, -2, 2), ncol=input_dims)
  }

  z <- W%*%matrix(x[1:input_dims], nrow=input_dims)
  y <- sum(sin((2*(1:5) + 1)*pi*z))/20
  return(y)
}

quackquack_ripples <- function(){
  out <- list(input_dim=2)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(0,0), c(1,1))
  rownames(RR) <- c("x1", "x2")
  out$input_range <- RR

  return(out)
}

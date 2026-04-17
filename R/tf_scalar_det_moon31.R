#' Moon et al. 31D Function
#'
#' A 31-dimensional benchmark function built from four existing physical-model
#' functions: Borehole (8D), Wing Weight (10D), OTL Circuit (6D), and Piston
#' (7D).
#'
#' By default, this implementation matches the Simon Fraser library version:
#' the four component outputs are rescaled within each input by subtracting
#' their minimum and dividing by their range, and the final output is the sum
#' of these four scaled values.
#'
#' @param x Input vector of length at least 31.
#' @param scale01 #' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param normalize_components Logical; if \code{FALSE} (default), use the
#'   Simon Fraser construction. If \code{TRUE}, use fixed componentwise
#'   normalization via the supplied means and standard deviations.
#'
#' @return Scalar output.
#' @details
#' The default behavior matches the Simon Fraser implementation of the Moon et
#' al. function. The optional fixed normalization is provided only for
#' convenience if a more stable cross-input scaling is desired.
#'
#' @references
#' Moon, H., Dean, A. M., & Santner, T. J. (2012). Two-stage sensitivity-based
#' group screening in computer experiments. \emph{Technometrics}, 54(4),
#' 376--387.
#'
#' @export
#' @examples
#' fname <- "moon31"
#' n <- 10
#' p <- quack(fname, verbose=FALSE)$input_dim
#' x <- matrix(runif(n*p), nrow=n)
#' y <- eval_duq(fname, x)
moon31 <- function(x, scale01 = TRUE,
                   normalize_components = FALSE) {
  if(scale01){
    RR <- quackquack_moon31()$input_range
    x[1:31] <- x[1:31] * (RR[,2] - RR[,1]) + RR[,1]
  }

  comp_mean <- c(77.57259, 268.1287, 5.431897, 0.4619819)
  comp_sd   <- c(45.54954, 48.41515, 1.133332, 0.1390398)

  y <- numeric(4)
  y[1] <- borehole(x[1:8], scale01 = FALSE)
  y[2] <- wingweight(x[9:18], scale01 = FALSE)
  y[3] <- circuit(x[19:24], scale01 = FALSE)
  y[4] <- piston(x[25:31], scale01 = FALSE)



  if (normalize_components) {
    y <- (y - comp_mean) / comp_sd
  }
  miny <- min(y)
  maxy <- max(y)

  if (maxy == miny) {
    return(0)
  }

  ystar <- (y - miny) / (maxy - miny)
  sum(ystar)
}

#' @export
quackquack_moon31 <- function(){
  out <- list(input_dim = 31)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- rbind(
    quackquack_borehole()$input_range,
    quackquack_wingweight()$input_range,
    quackquack_circuit()$input_range,
    quackquack_piston()$input_range
  )
  rownames(RR) <- paste0("x", 1:31)
  out$input_range <- RR
  out
}

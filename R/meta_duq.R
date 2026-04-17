#' @title Evaluate a UQ Test Function
#'
#' @description
#' Evaluate a `duqling` test function row-wise on a matrix of inputs.
#' The argument `f` may be either a function or a character string giving the
#' name of a function, typically from the `duqling` package.
#'
#' @name eval_duq
#' @aliases duq
#'
#' @param f A function or a character string giving the name of a function,
#'   usually from the `duqling` package.
#' @param x A numeric matrix with `n` rows and `quack()$input_dim` columns.
#'   If a vector is supplied, it is treated as a single input.
#' @param z An optional matrix of categorical inputs with `n` rows and
#'   `quack()$input_cat_dim` columns. Categories should be coercible to integers
#'   between 1 and the number of levels. If a vector is supplied, it is treated
#'   as a single input.
#' @param scale01 When `TRUE`, inputs are assumed to lie on the unit interval
#'   and are internally mapped to their native range. Default is `TRUE`.
#' @param ... Additional arguments passed to `f`.
#'
#' @return
#' The result of evaluating `f` row-wise over `x` (and `z`, if applicable).
#' For univariate test functions this is typically a numeric vector. For
#' multivariate or functional outputs, the return type matches the output of `f`.
#'
#' @details
#' This is a convenience wrapper for calling test functions in `duqling`.
#'
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation
#' experiments: test functions and datasets." Simon Fraser University,
#' Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' @export
#' @examples
#' n <- 100
#' p <- 2
#' X <- lhs::randomLHS(n, p)
#' f_list <- quack(input_dim = 2)$fname
#' eval_duq(f_list[1], X)
eval_duq <- function(f, x, z = NULL, scale01 = TRUE, ...) {
  if (is.null(nrow(x))) {
    x <- matrix(x, ncol = 1)
  }

  if (!is.null(z) && is.null(nrow(z))) {
    z <- matrix(z, ncol = 1)
  }

  has_cat <- FALSE
  response_type <- "univariate"

  if (is.character(f)) {
    qq <- quack(f, verbose = FALSE)
    has_cat <- isTRUE(qq$has_categorical)
    response_type <- qq$response

    # Look for f in duqling first, then in the calling environment
    f <- tryCatch(
      get(f, envir = asNamespace("duqling")),
      error = function(e) get(f, envir = parent.frame())
    )
  }

  if (!has_cat) {
    res <- apply(x, 1, f, scale01 = scale01, ...)
    return(res)
  }

  if (is.null(z)) {
    stop("This function requires categorical inputs `z`, but `z` was not provided.",
         call. = FALSE)
  }

  if (nrow(z) != nrow(x)) {
    stop("`x` and `z` must have the same number of rows.", call. = FALSE)
  }

  if (response_type == "univariate") {
    res <- vapply(
      seq_len(nrow(x)),
      function(i) f(x[i, ], z[i, ], scale01 = scale01, ...),
      numeric(1)
    )
  } else {
    test <- f(x[1, ], z[1, ], scale01 = scale01, ...)
    res <- vapply(
      seq_len(nrow(x)),
      function(i) f(x[i, ], z[i, ], scale01 = scale01, ...),
      test
    )
  }

  res
}

#' @rdname eval_duq
#' @export
duq <- eval_duq

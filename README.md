
<!-- README.md is generated from README.Rmd. Please edit that file -->

# duqling: A Package for Reproducible UQ Research

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![](https://img.shields.io/badge/devel%20version-1.0.1-purple.svg)](https://github.com/knrumsey/duqling)

<div class="figure">

<img src="inst/logos/DUQLING.png" alt="This logo was designed by Imagine AI Art Studio" width="50%" />
<p class="caption">
This logo was designed by Imagine AI Art Studio
</p>

</div>

### Description

The `duqling` R package contains a wide variety of test functions for
UQ. The goal of `duqling` is to facilitate reproducible UQ research by
providing a large number of popular test functions which

1.  Can be called quickly and easily in the exact same manner every time
2.  Allow for inputs to be generated on the unit hyper-rectangel. All
    `duqling` functions have (optional) internal scaling.

Additionally, the package provides a `run_simulation_study()` function
which makes it easy to perform simulation studies for emulators (or
non-linear regressors). This package makes it easier than ever to
compare the performance on methods, even across papers!

## Installation

To install the `duqling` package, type

``` r
# install.packages("devtools")
devtools::install_github("knrumsey/duqling")
library(duqling)
```

## Functions Included in Test Library

A master list of all functions found in the `duqling` package can be
found with the command

``` r
duqling::quack()
#>                    fname input_dim input_cat response_type stochastic
#> 1               const_fn         1     FALSE           uni          n
#> 2                 grlee1         1     FALSE           uni          n
#> 3                 banana         2     FALSE           uni          n
#> 4           dms_additive         2     FALSE           uni          n
#> 5        dms_complicated         2     FALSE           uni          n
#> 6           dms_harmonic         2     FALSE           uni          n
#> 7             dms_radial         2     FALSE           uni          n
#> 8             dms_simple         2     FALSE           uni          n
#> 9             foursquare         2     FALSE           uni          n
#> 10                grlee2         2     FALSE           uni          n
#> 11    lim_non_polynomial         2     FALSE           uni          n
#> 12        lim_polynomial         2     FALSE           uni          n
#> 13               micwicz         2     FALSE           uni          n
#> 14               ripples         2     FALSE           uni          n
#> 15              squiggle         2     FALSE           uni          n
#> 16         twin_galaxies         2     FALSE           uni          n
#> 17             const_fn3         3     FALSE           uni          n
#> 18          detpep_curve         3     FALSE           uni          n
#> 19              sharkfin         3     FALSE           uni          n
#> 20        simple_machine         3     FALSE          func          n
#> 21                 vinet         3     FALSE          func          n
#> 22            ocean_circ         4     FALSE           uni          y
#> 23             pollutant         4     FALSE          func          n
#> 24         pollutant_uni         4     FALSE           uni          n
#> 25              friedman         5     FALSE           uni          n
#> 26     simple_machine_cm         5     FALSE          func          n
#> 27     stochastic_piston         5     FALSE           uni          y
#> 28               circuit         6     FALSE           uni          n
#> 29                grlee6         6     FALSE           uni          n
#> 30                piston         7     FALSE           uni          n
#> 31              borehole         8     FALSE           uni          n
#> 32 borehole_low_fidelity         8     FALSE           uni          n
#> 33               detpep8         8     FALSE           uni          n
#> 34                 robot         8     FALSE           uni          n
#> 35              dts_sirs         9     FALSE          func          y
#> 36            friedman10        10     FALSE           uni          n
#> 37            wingweight        10     FALSE           uni          n
#> 38            const_fn15        15     FALSE           uni          n
#> 39            friedman20        20     FALSE           uni          n
#> 40               welch20        20     FALSE           uni          n
```

A list of all functions meeting certain criterion can be found with the
command

``` r
duqling::quack(input_dim=4:7, stochastic="n")
#>                fname input_dim input_cat response_type stochastic
#> 23         pollutant         4     FALSE          func          n
#> 24     pollutant_uni         4     FALSE           uni          n
#> 25          friedman         5     FALSE           uni          n
#> 26 simple_machine_cm         5     FALSE          func          n
#> 28           circuit         6     FALSE           uni          n
#> 29            grlee6         6     FALSE           uni          n
#> 30            piston         7     FALSE           uni          n
```

A detailed description of each function (the `borehole()` function, for
example) can be found with the command

``` r
duqling::quack("borehole")
#> $input_dim
#> [1] 8
#> 
#> $input_cat
#> [1] FALSE
#> 
#> $response_type
#> [1] "uni"
#> 
#> $input_range
#>        [,1]      [,2]
#> rw     0.05      0.15
#> r    100.00  50000.00
#> Tu 63070.00 115600.00
#> Hu   990.00   1110.00
#> Tl    63.10    116.00
#> Hl   700.00     80.00
#> L   1120.00   1680.00
#> Kw  9855.00  12045.00
```

## Using The Package

Every function in the `duqling` package will (optionally) perform
internal scaling so that inputs can be passed on a $(0, 1)$ scale for
simplicity. This internal scaling is turned on by setting the argument
`scale01=TRUE`. See help files for each individual function for details.

``` r
n <- 100
p <- 8
X <- matrix(runif(n*p), nrow=n, ncol=p)

# This is how duqling functions are generally called
y <- duqling::duq(X, "borehole")

# Or equivalently
y <- apply(X, 1, duqling::borehole, scale01=TRUE) 
```

## Reproducible Simulation Studies

To execute a reproducible simulation study, simply write two functions
`my_fit()` and `my_pred()` (or lump them both into one function) and
call the `run_sim_study()` function with your desired settings. Here’s
an example using the `BASS` package.

If both `my_fit()` and `my_pred()` are given, the package will time
these steps separately.

``` r
library(BASS)
#' This function should take arguments
#'    X_train
#'    y_train 
#' and return a fitted model
my_fit <- function(X, y){
  bass(X, y, h1=4, h2=40/nrow(X), verbose=FALSE)
}
```

The value returned by the `my_pred()` function depends on whether
interval estimation is desired. If so, `interval = TRUE`, then the
function should return a matrix with three columns representing (1)
point predictions (2) lower bound of interval estimate and (3) upper
bound of interval estimate. If `interval = FALSE`, then the function can
return only the first column (or a numeric vector).

``` r
#' This function should take arguments
#'    fitted_obj
#'    X_test
#'    conf_level (only if interval = TRUE) 
#' and return a matrix of predictions (see above for details)
my_pred <- function(obj, X_test, conf_level=0.95){
  alpha <- 1 - conf_level
  preds <- predict(obj, Xt)
  
  # Get point prediction
  yhat <- apply(preds, 2, mean)
  
  # Get uncertainty
  sd <- sqrt(apply(preds, 2, var) + mean(obj$s2)) 
  
  # Make matrix to be returned
  res <- cbind(yhat,          # Point estimate
               yhat - 2*sd,   # Lower bound
               yhat + 2*sd)   # Upper bound
  
  return(res)
}
```

Alternatively, everything can be lumped into the `my_fit()` function,
but only the total time (for fitting and prediction) will be computed.
Here’s an example with the `BART` package.

``` r
library(BART)
my_fit <- function(X_train, y_train, X_test, conf_level=0.95){
  mod <- wbart(X_train, y_train, X_test)
  yhat <- mod$yhat.test
  sd_post_mean <- apply(mod$yhat.test, 2, sd)
  sd_post_eps  <- mean(mod$sigma)
  sd <- sqrt(sd_post_mean^2 + sd_post_eps^2)
  cbind(yhat,
        yhat - 2*sd,
        yhat + 2*sd)
}

my_pred <- NULL
```

## Simulation Study Settings

When `run_sim_study` is called, each combination of the following
arguments will be run for `replications` replications.

- `fnames`: a vector of functions to test on. Should be a subset of
  `quack(input_cat=FALSE, stochastic="n", response_type="uni")$fname`.
- `n_train`: a vector of sample sizes.
- `NSR`: Noise to Signal ratio. The inverse of the more-standard SNR.
- `design_type`: What type of designs should be generated. Options are
  “LHS” (maximin when $n\leq 2000$, random otherwise), “grid” and
  “uniform”.

An example call would be

``` r
my_data <- run_sim_study(my_fit, my_pred,
                         fnames = c("dms_additive", "borehole", "welch20"),
                         interval = TRUE,
                         n_train = c(100, 500),
                         NSR = c(0, 1/5),
                         design_type = "LHS",
                         replications = 5)
```

Some suggested function vectors for simulation studies are included in
the functions

``` r
get_sim_functions_full()     #31 functions
get_sim_functions_medium()   #20 functions
get_sim_functions_short()    #10 functions
get_sim_functions_tiny()     #4  functions
get_sim_functions_2d()       #12 functions
```

### To Do List

1.  Add rigorous argument handling.
2.  Add functions to assess performance
3.  Run/store results of a simulation study

### Copyright Notice

*2023. Triad National Security, LLC. All rights reserved.*

*This program was produced under U.S. Government contract
89233218CNA000001 for Los Alamos National Laboratory (LANL), which is
operated by Triad National Security, LLC for the U.S. Department of
Energy/National Nuclear Security Administration. All rights in the
program are. reserved by Triad National Security, LLC, and the U.S.
Department of Energy/National Nuclear Security Administration. The
Government is granted for itself and others acting on its behalf a
nonexclusive, paid-up, irrevocable worldwide license in this material to
reproduce, prepare. derivative works, distribute copies to the public,
perform publicly and display publicly, and to permit others to do so.*

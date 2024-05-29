
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
providing a large number of test functions, datasets, and automated
simulation studies. The main functionality of `duqling` includes.

1.  A large library of test functions (see `quack()` function) with
    consistent usage and (optional) internal scaling which allows for
    inputs to be generated on the unit hyper-rectangle.
2.  A large library of UQ datasets (see `data_quack()` function) which
    are stored in the affiliated
    [UQDataverse](https://dataverse.harvard.edu/dataverse/UQdataverse/).
3.  Automated simulation studies for emulation (see `run_sim_study()`
    for test functions and `run_sim_study_data()` for datasets) which
    internally generates unique random seeds, allowing for direct
    comparison of results across papers and time.

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
#>                    fname input_dim input_cat response_type stochastic    noise
#> 1               const_fn         1     FALSE           uni          n   0.0000
#> 2                 grlee1         1     FALSE           uni          n   1.3059
#> 3                 banana         2     FALSE           uni          n 360.2585
#> 4           dms_additive         2     FALSE           uni          n   1.0061
#> 5        dms_complicated         2     FALSE           uni          n   1.0001
#> 6           dms_harmonic         2     FALSE           uni          n   0.9132
#> 7             dms_radial         2     FALSE           uni          n   1.0098
#> 8             dms_simple         2     FALSE           uni          n   0.9708
#> 9             foursquare         2     FALSE           uni          n   0.3843
#> 10                grlee2         2     FALSE           uni          n   0.0847
#> 11    lim_non_polynomial         2     FALSE           uni          n   1.9162
#> 12        lim_polynomial         2     FALSE           uni          n   1.6335
#> 13               micwicz         2     FALSE           uni          n   0.3547
#> 14               ripples         2     FALSE           uni          n   0.0797
#> 15           simple_poly         2     FALSE           uni          n   0.4775
#> 16              squiggle         2     FALSE           uni          n   0.5098
#> 17         twin_galaxies         2     FALSE           uni          n   1.0119
#> 18             const_fn3         3     FALSE           uni          n   0.0000
#> 19          detpep_curve         3     FALSE           uni          n  35.8406
#> 20              ishigami         3     FALSE           uni          n   3.7212
#> 21              sharkfin         3     FALSE           uni          n   0.2873
#> 22        simple_machine         3     FALSE          func          n       NA
#> 23                 vinet         3     FALSE          func          n       NA
#> 24            ocean_circ         4     FALSE           uni          y  13.5659
#> 25             pollutant         4     FALSE          func          n       NA
#> 26         pollutant_uni         4     FALSE           uni          n   0.8257
#> 27              friedman         5     FALSE           uni          n   4.8579
#> 28     simple_machine_cm         5     FALSE          func          n       NA
#> 29     stochastic_piston         5     FALSE           uni          y   0.1356
#> 30               circuit         6     FALSE           uni          n   1.1740
#> 31                grlee6         6     FALSE           uni          n   0.7268
#> 32                piston         7     FALSE           uni          n   0.1357
#> 33              borehole         8     FALSE           uni          n 112.6912
#> 34 borehole_low_fidelity         8     FALSE           uni          n  89.6763
#> 35               detpep8         8     FALSE           uni          n  36.3133
#> 36                 robot         8     FALSE           uni          n   0.5275
#> 37              dts_sirs         9     FALSE          func          y       NA
#> 38            friedman10        10     FALSE           uni          n   4.8579
#> 39            wingweight        10     FALSE           uni          n  46.7888
#> 40            const_fn15        15     FALSE           uni          n   0.0000
#> 41            friedman20        20     FALSE           uni          n   4.8579
#> 42               welch20        20     FALSE           uni          n   2.0899
```

A list of all functions meeting certain criterion can be found with the
command

``` r
duqling::quack(input_dim=4:7, stochastic="n")
#>                fname input_dim input_cat response_type stochastic  noise
#> 25         pollutant         4     FALSE          func          n     NA
#> 26     pollutant_uni         4     FALSE           uni          n 0.8257
#> 27          friedman         5     FALSE           uni          n 4.8579
#> 28 simple_machine_cm         5     FALSE          func          n     NA
#> 30           circuit         6     FALSE           uni          n 1.1740
#> 31            grlee6         6     FALSE           uni          n 0.7268
#> 32            piston         7     FALSE           uni          n 0.1357
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
results <- run_sim_study(my_fit, my_pred,
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

## Datasets and duqling

All data sets are hosted at our affiliated
[UQDataverse](https://dataverse.harvard.edu/dataverse/UQdataverse/). For
a full list of the unprocessed datasets, type

``` r
duqling::data_quack(raw=TRUE)
#>             dname input_dim output_dim      n input_cat_dim
#> 4            e3sm         2          1  48602             1
#> 3  stochastic_sir         4          1   2000             0
#> 2         pbx9501         6         10   7000             1
#> 1 strontium_plume        20         10    300             0
#> 5    fair_climate        46          1 168168             1
```

Some datasets have been processed for univariate emulation. These can be
found with the command

``` r
duqling::data_quack()
#>                             dname input_dim input_cat_dim     n
#> 8                            e3sm         2             0 48602
#> 7                  stochastic_sir         4             0  2000
#> 3                    pbx9501_gold         6             0   500
#> 4                   pbx9501_ss304         6             0   500
#> 5                  pbx9501_nickel         6             0   500
#> 6                 pbx9501_uranium         6             0   500
#> 1             strontium_plume_p4b        20             0   300
#> 2            strontium_plume_p104        20             0   300
#> 12 fair_climate_ssp1-2.6_year2200        45             0  1001
#> 13 fair_climate_ssp2-4.5_year2200        45             0  1001
#> 14 fair_climate_ssp3-7.0_year2200        45             0  1001
#> 9           fair_climate_ssp1-2.6        46             0 56056
#> 10          fair_climate_ssp2-4.5        46             0 56056
#> 11          fair_climate_ssp3-7.0        46             0 56056
```

Raw datasets can be read (via the internet) with the function
`get_UQ_data()` and the processed data with the function
`get_emulation_data()`. For example,

``` r
data <- duqling::get_UQ_data("strontium_plume")

data <- duqling::get_emulation_data("strontium_plume_p4b")
X <- data$X
y <- data$y
```

Akin to the `run_sim_study()` function, automated simulation studies can
be run with cross-validation using the datasets in duqling. See the
previous section on simulation studies for details.

``` r
results <- run_sim_study_data(my_fit, my_pred,
                dnames=c("pbx9501_gold", "strontium_plume_p104"),
                folds=c(5, 10))
```

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

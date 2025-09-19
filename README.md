
<!-- README.md is generated from README.Rmd. Please edit that file -->

# duqling: A Package for Reproducible UQ Research

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![](https://img.shields.io/badge/release%20version-2.1.0-blue.svg)](https://github.com/knrumsey/duqling)

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
#>                      fname input_dim     response has_categorical stochastic
#> 1                 const_fn         1   univariate           FALSE      FALSE
#> 2               forrester1         1   univariate           FALSE      FALSE
#> 3  forrester1_low_fidelity         1   univariate           FALSE      FALSE
#> 4                   grlee1         1   univariate           FALSE      FALSE
#> 5                   banana         2   univariate           FALSE      FALSE
#> 6             dms_additive         2   univariate           FALSE      FALSE
#> 7          dms_complicated         2   univariate           FALSE      FALSE
#> 8             dms_harmonic         2   univariate           FALSE      FALSE
#> 9               dms_radial         2   univariate           FALSE      FALSE
#> 10              dms_simple         2   univariate           FALSE      FALSE
#> 11              foursquare         2   univariate           FALSE      FALSE
#> 12                  grlee2         2   univariate           FALSE      FALSE
#> 13      lim_non_polynomial         2   univariate           FALSE      FALSE
#> 14          lim_polynomial         2   univariate           FALSE      FALSE
#> 15             multivalley         2   univariate           FALSE      FALSE
#> 16                 ripples         2   univariate           FALSE      FALSE
#> 17             simple_poly         2   univariate           FALSE      FALSE
#> 18                squiggle         2   univariate           FALSE      FALSE
#> 19                   star2         2   univariate           FALSE      FALSE
#> 20           twin_galaxies         2   univariate           FALSE      FALSE
#> 21               const_fn3         3   univariate           FALSE      FALSE
#> 22                   cube3         3   univariate           FALSE      FALSE
#> 23            cube3_rotate         3   univariate           FALSE      FALSE
#> 24            detpep_curve         3   univariate           FALSE      FALSE
#> 25               Gfunction         3   univariate           FALSE      FALSE
#> 26                ishigami         3   univariate           FALSE      FALSE
#> 27                 rabbits         3   univariate           FALSE      FALSE
#> 28                sharkfin         3   univariate           FALSE      FALSE
#> 29          simple_machine         3   functional           FALSE      FALSE
#> 30                   vinet         3   functional           FALSE      FALSE
#> 31              ocean_circ         4   univariate           FALSE       TRUE
#> 32                   park4         4   univariate           FALSE      FALSE
#> 33      park4_low_fidelity         4   univariate           FALSE      FALSE
#> 34               pollutant         4   functional           FALSE      FALSE
#> 35           pollutant_uni         4   univariate           FALSE      FALSE
#> 36         beam_deflection         5   functional           FALSE      FALSE
#> 37                   cube5         5   univariate           FALSE      FALSE
#> 38                friedman         5   univariate           FALSE      FALSE
#> 39            short_column         5   univariate           FALSE      FALSE
#> 40       simple_machine_cm         5   functional           FALSE      FALSE
#> 41       stochastic_piston         5   univariate           FALSE       TRUE
#> 42                 bs_call         6   functional           FALSE       TRUE
#> 43                  bs_put         6   functional           FALSE       TRUE
#> 44            cantilever_D         6   univariate           FALSE      FALSE
#> 45            cantilever_S         6   univariate           FALSE      FALSE
#> 46                 circuit         6   univariate           FALSE      FALSE
#> 47              Gfunction6         6   univariate           FALSE      FALSE
#> 48                  grlee6         6   univariate           FALSE      FALSE
#> 49                  crater         7   univariate           FALSE      FALSE
#> 50               gamma_mix         7   univariate           FALSE      FALSE
#> 51                  piston         7   univariate           FALSE      FALSE
#> 52                borehole         8   univariate           FALSE      FALSE
#> 53   borehole_low_fidelity         8   univariate           FALSE      FALSE
#> 54                 detpep8         8   univariate           FALSE      FALSE
#> 55                   ebola         8   univariate           FALSE      FALSE
#> 56                   robot         8   univariate           FALSE      FALSE
#> 57                dts_sirs         9   functional           FALSE       TRUE
#> 58            steel_column         9   univariate           FALSE      FALSE
#> 59                  sulfur         9   univariate           FALSE      FALSE
#> 60              friedman10        10   univariate           FALSE      FALSE
#> 61                ignition        10   univariate           FALSE      FALSE
#> 62              wingweight        10   univariate           FALSE      FALSE
#> 63             Gfunction12        12   univariate           FALSE      FALSE
#> 64              const_fn15        15   univariate           FALSE      FALSE
#> 65                    oo15        15   univariate           FALSE      FALSE
#> 66                  permdb        16   univariate           FALSE      FALSE
#> 67             Gfunction18        18   univariate           FALSE      FALSE
#> 68              friedman20        20   univariate           FALSE      FALSE
#> 69                 welch20        20   univariate           FALSE      FALSE
#> 70            d_onehundred       100 multivariate           FALSE      FALSE
#> 71              onehundred       100   univariate           FALSE      FALSE
```

A list of all functions meeting certain criterion can be found with the
command

``` r
duqling::quack(input_dim=4:7, stochastic="n")
#>                 fname input_dim   response has_categorical stochastic
#> 1               park4         4 univariate           FALSE      FALSE
#> 2  park4_low_fidelity         4 univariate           FALSE      FALSE
#> 3           pollutant         4 functional           FALSE      FALSE
#> 4       pollutant_uni         4 univariate           FALSE      FALSE
#> 5     beam_deflection         5 functional           FALSE      FALSE
#> 6               cube5         5 univariate           FALSE      FALSE
#> 7            friedman         5 univariate           FALSE      FALSE
#> 8        short_column         5 univariate           FALSE      FALSE
#> 9   simple_machine_cm         5 functional           FALSE      FALSE
#> 10       cantilever_D         6 univariate           FALSE      FALSE
#> 11       cantilever_S         6 univariate           FALSE      FALSE
#> 12            circuit         6 univariate           FALSE      FALSE
#> 13         Gfunction6         6 univariate           FALSE      FALSE
#> 14             grlee6         6 univariate           FALSE      FALSE
#> 15             crater         7 univariate           FALSE      FALSE
#> 16          gamma_mix         7 univariate           FALSE      FALSE
#> 17             piston         7 univariate           FALSE      FALSE
```

A detailed description of each function (the `borehole()` function, for
example) can be found with the command

``` r
duqling::quack("borehole")
#> Function: borehole 
#>   Input dimension: 8 
#>   Response type: univariate 
#>   Stochastic: No 
#>   Has categorical inputs: FALSE 
#> 
#> Input ranges:
#>        [,1]      [,2]
#> rw     0.05      0.15
#> r    100.00  50000.00
#> Tu 63070.00 115600.00
#> Hu   990.00   1110.00
#> Tl    63.10    116.00
#> Hl   700.00    820.00
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
                         method_names <- c("my_method"),
                         replications = 5)
```

Some suggested function vectors for simulation studies are included in
the functions

``` r
get_sim_functions_full()     #51 functions
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
#>             dname input_dim output_dim       n input_cat_dim
#> 1   Z_machine_exp         1          1   23224             3
#> 2            e3sm         2          1   48602             1
#> 3  stochastic_sir         4          1    2000             0
#> 4         pbx9501         6         10    7000             1
#> 5  flyer_plate104        11        200    1000             0
#> 6 strontium_plume        20         10     300             0
#> 7   Z_machine_sim        40          9 5000000             0
#> 8    fair_climate        46          1  168168             1
```

Some datasets have been processed for univariate emulation. These can be
found with the command

``` r
duqling::data_quack()
#>                             dname input_dim input_cat_dim     n response_type
#> 1                            e3sm         2             0 48602           uni
#> 2                       e3sm_mcar         2             0 10000           uni
#> 3                       e3sm_mnar         2             0  9122           uni
#> 4                  stochastic_sir         4             0  2000           uni
#> 5                       SLOSH_low         5             0  4000           uni
#> 6                       SLOSH_mid         5             0  4000           uni
#> 7                      SLOSH_high         5             0  4000           uni
#> 8                    pbx9501_gold         6             0   500           uni
#> 9                   pbx9501_ss304         6             0   500           uni
#> 10                 pbx9501_nickel         6             0   500           uni
#> 11                pbx9501_uranium         6             0   500           uni
#> 12             Z_machine_max_vel1         6             0  5000           uni
#> 13             Z_machine_max_vel2         6             0  5000           uni
#> 14                 flyer_plate104        11             0  1000           uni
#> 15            strontium_plume_p4b        20             0   300           uni
#> 16           strontium_plume_p104        20             0   300           uni
#> 17          Z_machine_max_vel_all        30             0  5000           uni
#> 18 fair_climate_ssp1-2.6_year2200        45             0  1001           uni
#> 19 fair_climate_ssp2-4.5_year2200        45             0  1001           uni
#> 20 fair_climate_ssp3-7.0_year2200        45             0  1001           uni
#> 21          fair_climate_ssp1-2.6        46             0 56056           uni
#> 22          fair_climate_ssp2-4.5        46             0 56056           uni
#> 23          fair_climate_ssp3-7.0        46             0 56056           uni
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

## Analysis and visualization

The `duqling` package provides many tools for analyzing emulator results
and generating publication-ready figures. An example analysis proceeds
as follows:

``` r
# Process your sim study results from before
duq_obj <- process_sim_study(results)

# Load in data from the big simulation study (Rumsey et al 2025; coming soon)
load("data/existing_results.Rda") # --> results_paper

# Now you can join your results together!
duq_obj <- join_sim_study(duq_obj,
                          process_sim_study(results_paper))

# Filter down to just the results you want
duq_obj_sub <- filter_sim_study(duq_obj, 
                                id = c("dms_additive", "borehole", "welch20"),
                                n_train = 500,
                                NSR = 0,
                                method = c("my_method", "gp", "bnn", "confrf"))

# Now you can make plots and summaries!
summarize_sim_study(duq_obj_sub, summarize = c("time", "CRPS"))
rankplot_sim_study(duq_obj_sub, "RMSE")
heatmap_sim_study(duq_obj_sub, "CRPS")
paretoplot_sim_study(duq_obj_sub, c("CRPS_rel", "time_rel_log"))

# View boxplots
boxplots_sim_study(filter_sim_study(duq_obj_sub, id="borehole"), "CRPS_norm")
```

### Release Information

LA-UR-25-27410

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

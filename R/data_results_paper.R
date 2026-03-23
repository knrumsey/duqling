#' Benchmark results from the emulator comparison paper
#'
#' Results from the large-scale simulation study presented in
#' "All Emulators are Wrong, Many are Useful, Some are More Useful Than Others:
#' A Reproducible Comparison of Computer Model Surrogates".
#'
#' The `results_paper` dataset contains benchmark results for emulator
#' comparisons on built-in test functions included in \pkg{duqling}. These
#' results are intended to support reproducible analyses, examples, and
#' visualizations accompanying the paper.
#'
#' The dataset typically includes identifiers describing the simulation setting
#' (for example emulator name, benchmark function, training size, replication,
#' and noise level), along with performance summaries such as prediction error,
#' interval coverage, scoring rules, and timing information.
#'
#' @format A data frame or tibble containing benchmark results from simulation
#'   studies on synthetic test functions. The number of rows and variables
#'   depends on the version of the package.
#' \describe{
#'   \item{emulator}{Name of the emulator or surrogate method.}
#'   \item{fname}{Name of the benchmark test function.}
#'   \item{n_train}{Training sample size used for fitting.}
#'   \item{NSR}{Noise-to-signal ratio used in the experiment.}
#'   \item{replication}{Replication index for repeated experiments.}
#'   \item{rmse}{Root mean squared prediction error.}
#'   \item{crps}{Continuous ranked probability score.}
#'   \item{coverage}{Empirical prediction interval coverage.}
#'   \item{train_time}{Training time for the emulator fit.}
#'   \item{pred_time}{Prediction time for the emulator.}
#' }
#'
#' Not all variables listed above are guaranteed to appear in every version; the
#' exact columns depend on the simulation study configuration used to generate
#' the stored results.
#'
#' @usage data(results_paper)
#' @source Generated using \pkg{duqling} simulation study workflows.
#' @references Rumsey, K., Francom, D., Gibson, C., and Morris, R.
#'   "All Emulators are Wrong, Many are Useful, Some are More Useful Than
#'   Others: A Reproducible Comparison of Computer Model Surrogates."
#' @examples
#' data(results_paper)
#' head(results_paper)
#'
#' if ("emulator" %in% names(results_paper)) {
#'   table(results_paper$emulator)
#' }
#' @name results_paper
NULL


#' Benchmark results from real-data emulator comparisons
#'
#' Results from cross-validation studies on real-world uncertainty
#' quantification datasets used in the emulator comparison paper.
#'
#' The `results_data_paper` dataset contains benchmark results for emulator
#' comparisons on curated real datasets distributed with or accessed through
#' \pkg{duqling}. These results are intended to support reproducible analyses,
#' examples, and visualizations accompanying the paper.
#'
#' The dataset typically includes identifiers describing the dataset, emulator,
#' fold, and experimental configuration, together with predictive performance,
#' uncertainty quantification metrics, and timing summaries.
#'
#' @format A data frame or tibble containing benchmark results from
#'   cross-validation studies on real datasets. The number of rows and variables
#'   depends on the version of the package.
#' \describe{
#'   \item{emulator}{Name of the emulator or surrogate method.}
#'   \item{dname}{Name of the real dataset.}
#'   \item{fold}{Cross-validation fold index.}
#'   \item{rmse}{Root mean squared prediction error.}
#'   \item{crps}{Continuous ranked probability score.}
#'   \item{coverage}{Empirical prediction interval coverage.}
#'   \item{train_time}{Training time for the emulator fit.}
#'   \item{pred_time}{Prediction time for the emulator.}
#' }
#'
#' Not all variables listed above are guaranteed to appear in every version; the
#' exact columns depend on the study design used to generate the stored results.
#'
#' @usage data(results_data_paper)
#' @source Generated using \pkg{duqling} data-based simulation study workflows.
#' @references Rumsey, K., Francom, D., Gibson, C., and Morris, R.
#'   "All Emulators are Wrong, Many are Useful, Some are More Useful Than
#'   Others: A Reproducible Comparison of Computer Model Surrogates."
#' @examples
#' data(results_data_paper)
#' head(results_data_paper)
#'
#' if ("dname" %in% names(results_data_paper)) {
#'   unique(results_data_paper$dname)
#' }
#' @name results_data_paper
NULL

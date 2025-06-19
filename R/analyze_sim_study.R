#' Plot paired metric differences between two methods (base graphics)
#'
#' For every test function and replicate the difference
#'   \eqn{d = \text{metric}_{\text{m1}} - \text{metric}_{\text{m2}}}
#' is computed.  The per-function average is drawn as a vertical segment
#' (blue when \code{method1} is better, red otherwise) and individual
#' replicate differences are shown as jittered points of matching colour.
#'
#' A **signed–log (modulus)** transform is applied so large positive/negative
#' values are compressed while small differences stay legible.  Setting
#' \code{bend = 1} turns the transform off (ordinary linear axis).
#'
#' @param df         Data-frame that contains at least the columns
#'   \code{method}, \code{fname}, \code{rep} and the chosen metric.
#' @param metric_col Name of the metric column (character).  Default
#'   \code{"FVU"}.
#' @param method1    First method (default \code{"method1"}).
#' @param method2    Second method (default \code{"method2"}).
#' @param bend       Bending parameter \eqn{0<p\le 1} for Tukey’s modulus
#'   transform; Default \code{bend = 1} is equivalent to no transform.
#'
#' @return Invisibly returns the data frame of differences used for plotting.
#' @examples
#' ## plot_metric_diffs_base(results, "FVU", "khaos", "bass")
plot_metric_diffs <- function(df,
                                   metric_col = "FVU",
                                   method1    = "method1",
                                   method2    = "method2",
                                   bend       = 1.0) {

  stopifnot(metric_col %in% names(df),
            all(c("method", "fname", "rep") %in% names(df)),
            bend > 0, bend <= 1)

  if(length(unique(data$method)) == 1){
    stop("Need at least two methods for a comparison")
  }

  ## ── helper: Tukey modulus transform ────────────────────────────
  mod_trans <- function(x, p) {
    if (p == 1) return(x)                       # identity
    sign(x) * (((abs(x) + 1)^p - 1) / p)
  }

  ## ── reshape to paired differences ─────────────────────────────
  wide <- subset(df, method %in% c(method1, method2))[,
                                                      c("fname", "rep", "method", metric_col)]

  # wide data
  wide <- reshape(wide, idvar = c("fname", "rep"),
                  timevar = "method", direction = "wide")

  # drop incomplete pairs
  wide <- wide[ complete.cases(wide), ]

  # difference: metric.m1 - metric.m2
  dcol1 <- paste(metric_col, method1, sep = ".")
  dcol2 <- paste(metric_col, method2, sep = ".")
  wide$diff <- wide[[dcol1]] - wide[[dcol2]]

  ## per-function average
  avg <- aggregate(diff ~ fname, data = wide, mean)
  avg <- avg[order(avg$diff), ]                 # order by average
  wide$fname <- factor(wide$fname, levels = avg$fname)
  avg$fname  <- factor(avg$fname , levels = avg$fname)

  ## colours
  better <- avg$diff < 0
  col_fun <- ifelse(better, "#377eb8", "#e41a1c")   # blue / red
  names(col_fun) <- as.character(avg$fname)

  ## ── base-graphics plot ─────────────────────────────────────────
  y_trans <- mod_trans(wide$diff, bend)
  y_avg   <- mod_trans(avg$diff , bend)

  ylim <- range(c(y_trans, 0))
  nfun <- nrow(avg)

  op <- par(mar = c(8, 5, 4, 2) + 0.1)
  on.exit(par(op), add = TRUE)

  plot.new()
  plot.window(xlim = c(0.5, nfun + 0.5), ylim = ylim)
  abline(h = 0, col = "grey50")

  ## segments for averages
  for (i in seq_len(nfun)) {
    lines(i * c(1, 1), c(0, y_avg[i]),
          col = col_fun[i], lwd = 2)
  }

  ## jittered replicate points
  jitter_x <- as.numeric(wide$fname) +
    runif(nrow(wide), -0.15, 0.15)
  points(jitter_x, mod_trans(wide$diff, bend),
         pch = 20, cex = 0.6,
         col = col_fun[as.character(wide$fname)])

  ## axes & labels
  axis(1, at = seq_len(nfun), labels = avg$fname, las = 2, cex.axis = 0.8)
  axis(2)
  title(main = sprintf("%s vs %s: %s differences by function",
                       method1, method2, metric_col),
        ylab = sprintf("%s difference  (%s − %s)",
                       metric_col, method1, method2),
        xlab = "")
  box()

  invisible(wide)
}



#' Plot cumulative rank-distribution curves for competing emulation methods
#'
#' For every *case* in the data set (a unique combination of
#' `fname`, `input_dim`, `n`, `NSR`, `design_type`, and `rep`) the
#' supplied performance `metric` is ranked across the competing
#' `method`s (rank 1 = best).
#' The function draws, for each method, the cumulative percentage of
#' cases whose rank is *≤ r* ( r = 1, 2, …)—a so-called “bathtub”
#' or empirical cumulative rank plot.
#'
#' @param data   a data frame containing at least the columns
#'   `method`, `fname`, `input_dim`, `n`, `NSR`, `design_type`, `rep`,
#'   and the chosen `metric`.
#' @param metric character string naming the performance column to use
#'   (e.g. `"FVU"`, `"CRPS"`, …).  Any numeric column present in
#'   `data` is allowed.
#' @param cols   an optional vector of colours (recycled if necessary).
#'   If `NULL` the base-R palette is used.
#'
#' @return Invisibly returns a list with the average ranks per method.
#' @examples
#' ## df is your benchmarking data frame ---------------------------------
#' plot_rank_distribution_base(df, metric = "FVU")
#' plot_rank_distribution_base(df, metric = "CRPS",
#'                             cols = c("steelblue", "firebrick"))
#' @export
plot_rank_distribution <- function(data, metric = "FVU", cols = NULL)
{
  if (!metric %in% names(data))
    stop("Column `", metric, "` not found")

  if(length(unique(data$method)) == 1){
    stop("Need at least two methods for a comparison")
  }

  ## -- ranks within each design-replication case ----------------------------
  id <- interaction(data$fname, data$input_dim, data$n,
                    data$NSR,  data$design_type, data$rep, drop = TRUE)
  data$rank <- ave(data[[metric]], id,
                   FUN = function(z) rank(z, ties.method = "average"))

  ## -- order methods by average rank ----------------------------------------
  mean_rank <- tapply(data$rank, data$method, mean)
  methods   <- names(sort(mean_rank))
  k         <- length(methods)
  max_r     <- max(data$rank)

  if (is.null(cols)) cols <- rep(1:6, length.out = k)

  ## -- layout: 1 row for plot, narrow row for legend ------------------------
  op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
  layout(matrix(1:2, nrow = 2), heights = c(0.85, 0.15))

  ## main panel
  par(mar = c(4.5, 4.5, 3.5, 2))
  plot(NA, xlim = c(1, max_r), ylim = c(0, 100),
       xlab = "Rank or better", ylab = "Percentage of cases (%)",
       main = sprintf("Rank distribution (%s)", metric),
       xaxt = "n")
  axis(1, at = 1:max_r)

  for (i in seq_along(methods)) {
    y <- data$rank[data$method == methods[i]]
    pct <- sapply(1:max_r, function(r) mean(y <= r) * 100)
    lines(1:max_r, pct, type = "b", col = cols[i], pch = i, lwd = 2)
  }

  ## legend panel (blank plot, then legend)
  par(mar = c(0, 0, 0, 0))
  plot.new();  plot.window(xlim = 0:1, ylim = 0:1)
  legend("center", horiz = TRUE, bty = "n",
         legend = methods, col = cols,
         pch = seq_along(methods), lwd = 2)

  invisible(mean_rank[methods])
}

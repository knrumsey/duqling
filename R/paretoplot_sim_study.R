#' Pareto Plot of Accuracy vs Time
#'
#' Plot tradeoff between performance and runtime for each method using rank-based scores.
#'
#' @param df Data frame from \code{run_sim_study()}.
#' @param metric Length-2 character vector. The performance and runtime metrics to compare. Default is \code{c("CRPS", "t_tot")}.
#' @param methods Optional character vector of methods to include.
#' @param highlight_method Character vector of methods to label and color. Use \code{NA} to color all (default), or \code{NULL} to color none.
#' @param ties_method How to handle ties.
#' @param pin_axes Logical. Should axes be pinned at (0, 1) ?
#' @param show_legend Logical. Show legend?
#' @param show_pfront Logical. Should pareto-front be shown?
#' @param title Title for plot. Set to \code{FALSE} to suppress title.
#' @param path Optional file path to save plot (e.g., "figs/pareto_plot.png"). If NULL, the plot is returned instead.
#' @return A ggplot object.
#' @export
paretoplot_sim_study <- function(df,
                                 metric = c("CRPS", "t_tot"),
                                 methods = NULL,
                                 highlight_method = NA,
                                 ties_method = "average",
                                 pin_axes = TRUE,
                                 show_legend=TRUE,
                                 show_pfront=TRUE,
                                 title=NULL,
                                 path = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install 'ggplot2'.")

  # In case df comes from run_sim_study_data()
  if (!"fname" %in% names(df) && "dname" %in% names(df)) {
    df$fname <- df$dname
    df$replication <- df$fold
    df$n_train <- NA
    df$NSR <- NA
    df$design_type <- NA
  }

  perf_metric <- metric[1]
  time_metric <- metric[2]

  df[[perf_metric]] <- as.numeric(df[[perf_metric]])
  df[[time_metric]] <- as.numeric(df[[time_metric]])

  pretty_name_map <- c(
    "t_tot"  = "Speed",
    "t_pred" = "Prediction Speed",
    "t_fit"  = "Training Speed"
  )

  metric_pretty <- metric
  metric_pretty[metric %in% names(pretty_name_map)] <- pretty_name_map[metric[metric %in% names(pretty_name_map)]]

  # Filter methods
  if (!is.null(methods)) {
    df <- df[df$method %in% methods, , drop = FALSE]
  }

  # Define scenario ID
  scenario_cols <- c("fname", "n_train", "NSR", "design_type", "replication")
  df$scenario_id <- apply(df[, scenario_cols, drop = FALSE], 1, paste, collapse = "_")

  method_list <- sort(unique(df$method))
  K <- length(method_list)

  # Split by scenario
  scenario_list <- split(df, df$scenario_id)

  # Initialize rank accumulators
  rank_perf_sum <- setNames(rep(0, K), method_list)
  rank_time_sum <- setNames(rep(0, K), method_list)
  n_scenarios <- setNames(rep(0, K), method_list)

  for (scenario in scenario_list) {
    valid <- scenario$failure_type %in% c("none", "fit", "pred", "time")
    if (sum(valid) < 1) next

    valid_df <- scenario[valid, , drop = FALSE]
    methods_present <- valid_df$method

    ranks_perf <- rank(valid_df[[perf_metric]], ties.method = ties_method)
    ranks_time <- rank(valid_df[[time_metric]], ties.method = ties_method)

    for (i in seq_len(nrow(valid_df))) {
      m <- valid_df$method[i]
      rank_perf_sum[m] <- rank_perf_sum[m] + ranks_perf[i]
      rank_time_sum[m] <- rank_time_sum[m] + ranks_time[i]
      n_scenarios[m] <- n_scenarios[m] + 1
    }
  }

  # method_stats <- data.frame(
  #   method = method_list,
  #   avg_rank_perf = rank_perf_sum / n_scenarios,
  #   avg_rank_time = rank_time_sum / n_scenarios,
  #   n_scenarios = n_scenarios,
  #   stringsAsFactors = FALSE
  # )

  valid_methods <- names(n_scenarios[n_scenarios > 0])


  method_stats <- data.frame(
    method = valid_methods,
    avg_rank_perf = rank_perf_sum[valid_methods] / n_scenarios[valid_methods],
    avg_rank_time = rank_time_sum[valid_methods] / n_scenarios[valid_methods],
    n_scenarios = n_scenarios[valid_methods],
    stringsAsFactors = FALSE
  )

  # Convert to normalized [0,1] scores (1 = best)
  method_stats$score_perf <- 1 - (method_stats$avg_rank_perf - 1) / (K - 1)
  method_stats$score_time <- 1 - (method_stats$avg_rank_time - 1) / (K - 1)

  # Highlight logic
  if (is.na(highlight_method[1])) {
    method_stats$highlight <- TRUE
  } else if (is.null(highlight_method)) {
    method_stats$highlight <- FALSE
  } else {
    method_stats$highlight <- method_stats$method %in% highlight_method
  }

  #browser()

  # Pareto front detection (lower avg_rank is better)
  method_stats$avg_rank_perf <- rank_perf_sum[valid_methods] / n_scenarios[valid_methods]
  method_stats$avg_rank_time <- rank_time_sum[valid_methods] / n_scenarios[valid_methods]

  is_dominated <- function(i, mat) {
    any(mat[-i, "avg_rank_time"] <= mat[i, "avg_rank_time"] &
          mat[-i, "avg_rank_perf"] <= mat[i, "avg_rank_perf"] &
          (mat[-i, "avg_rank_time"] < mat[i, "avg_rank_time"] |
             mat[-i, "avg_rank_perf"] < mat[i, "avg_rank_perf"]))
  }

  pareto_idx <- which(!sapply(seq_len(nrow(method_stats)), function(i) is_dominated(i, method_stats)))
  method_stats$pareto <- FALSE
  method_stats$pareto[pareto_idx] <- TRUE

  pareto_df <- method_stats[method_stats$pareto, ]
  pareto_df <- pareto_df[order(pareto_df$score_time), ]

  # Build plot
  p <- ggplot2::ggplot(method_stats, ggplot2::aes(x = score_time, y = score_perf)) +
    ggplot2::geom_point(
      ggplot2::aes(color = highlight, shape = pareto),
      size = 3
    ) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "#1f78b4", "FALSE" = "gray60"),
      guide = "none"
    ) +
    ggplot2::theme_minimal(base_size = 14)


  if(show_pfront && nrow(pareto_df) > 1){
    p <- p + ggplot2::geom_path(
      data = pareto_df,
      ggplot2::aes(x = score_time, y = score_perf),
      color = "black",
      linewidth = 0.5,
      linetype = "dashed",
      inherit.aes = FALSE
    )
  }
  if(show_legend){
    p <- p + ggplot2::scale_shape_manual(
      values = c(16, 17),
      guide = ggplot2::guide_legend(title = "Pareto Front")
    )
  }else{
    p <- p + ggplot2::guides(shape = "none")
  }
  plot_title <- if (is.null(title)) {
    paste("Pareto Plot (Rank-Based):", metric[1], "vs", metric[2])
  } else if (identical(title, FALSE)) {
    NULL
  } else {
    title
  }
  p <- p + ggplot2::labs(
      #x = paste("Speed Score (1 = Fastest)", "\n← Slower     Faster →"),
      #y = paste("Accuracy Score (1 = Best)", "\n↓ Worse      Better ↑"),
      #x = paste0(metric[2], "\n← Worse     Better →"),
      #y = paste0(metric[1], "\n← Worse     Better →"),
      x = paste0(metric_pretty[2], "\n", "\u2190", " Worse     Better ", "\u2192"),
      y = paste0(metric_pretty[1], "\n", "\u2190", " Worse     Better ", "\u2192"),
      title = plot_title
    )

  # Pin axes
  if (pin_axes) {
    p <- p +
      ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
      ggplot2::coord_cartesian(clip = "off")
  } else {
    p <- p + ggplot2::coord_cartesian(clip = "off")
  }

  # Labels
  if (any(method_stats$highlight)) {
    p <- p + ggplot2::geom_text(
      data = method_stats[method_stats$highlight, ],
      ggplot2::aes(x = score_time, y = score_perf, label = method),
      vjust = -0.5,
      hjust = 0.5,
      size = 3.5,
      inherit.aes = FALSE
    )
  }

  # Save or return
  if (!is.null(path)) {
    ggplot2::ggsave(filename = path, plot = p, width = 12, height = 6, units = "in", dpi = 350)
    invisible(p)
  } else {
    return(p)
  }
}

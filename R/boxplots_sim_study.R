#' Boxplots for Emulator Comparison
#'
#' Create side-by-side boxplots of a performance metric across methods.
#'
#' @param df A data frame from \code{run_sim_study()}.
#' @param metric Character. The metric to plot (e.g., \code{"CRPS"}, \code{"RMSE"}, \code{"t_tot"}).
#' @param methods Optional. A character vector of methods to include.
#' @param group_by Optional. A character vector of grouping columns (e.g., \code{"fname"}, \code{"NSR"}).
#' @param log_scale_y Logical. Should the y-axis be log-scaled? Default is \code{FALSE}.
#' @param y_limit Optional. A numeric vector of length 2 to set y-axis limits.
#' @param show_mean Logical. Add a point for the group mean? Default is \code{FALSE}.
#' @param notches Logical. Should boxplot notches be shown? Default is \code{FALSE}.
#' @param show_outliers Logical. Show boxplot outliers? Default is \code{TRUE}.
#' @param sort_by How should methods be sorted on x-axis? Options are default \code{"performance"} or \code{"alphabet"}
#' @param title Optional title for plot. Set to \code{FALSE} to suppress title.
#' @param path Optional file path to save the figure. If NULL, returns a ggplot object.

#' @return A ggplot object or saves a figure to \code{path}.
#' @details
#' This function is primarily intended for visualizing filtered subsets of simulation output
#' (see \code{filter_sim_study()}). If multiple functions are present without grouping,
#' a warning is issued. Faceting by group is supported, but more than 8 groups may produce an unreadable plot.
#'
#' @examples
#' \dontrun{
#' filtered <- filter_sim_study(results, n_train = 1000, NSR = 0)
#' boxplots_sim_study(filtered, metric = "CRPS")
#'
#' boxplots_sim_study(filtered, metric = "t_tot", log_scale_y = TRUE, show_mean = TRUE)
#'
#' boxplots_sim_study(results, metric = "CRPS", group_by = c("fname", "NSR"))
#' }
#' @export
boxplots_sim_study <- function(df,
                               metric = "CRPS",
                               methods = NULL,
                               group_by = NULL,
                               log_scale_y = FALSE,
                               y_limit = NULL,
                               show_mean = FALSE,
                               notches = FALSE,
                               show_outliers = TRUE,
                               sort_by = "performance",
                               title = NULL,
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

  if (!metric %in% names(df)) stop(paste("Metric", metric, "not found in data."))

  if (!"fname" %in% group_by && "fname" %in% names(df)) {
    n_fnames <- length(unique(df$fname))
    if (n_fnames > 1) {
      warning(
        "Multiple 'fname' values detected but 'group_by' does not include 'fname'.\n",
        "Comparing metrics across functions may not be meaningful.\n",
        "Consider using filter_sim_study() to subset the data."
      )
    }
  }

  if (!sort_by %in% c("performance", "alphabet")) {
    stop("sort_by must be either 'performance' or 'alphabet'")
  }

  # Filter methods
  if (!is.null(methods)) {
    df <- df[df$method %in% methods, , drop = FALSE]
  }

  # Ensure numeric
  df[[metric]] <- as.numeric(df[[metric]])

  # Basic ggplot aes
  aes_args <- ggplot2::aes(x = method, y = .data[[metric]])

  # Collapse group_by to group_id
  facet_formula <- NULL
  if (!is.null(group_by)) {
    if (!all(group_by %in% names(df))) {
      stop("Some group_by columns not found in data.")
    }

    df$group_id <- apply(df[, group_by, drop = FALSE], 1, paste, collapse = " × ")
    facet_formula <- ggplot2::facet_grid(rows = ggplot2::vars(group_id))

    n_groups <- length(unique(df$group_id))
    if (n_groups > 8) {
      warning(
        "There are ", n_groups, " unique groups.\n",
        "This may result in an unreadable plot.\n",
        "Consider using filter_sim_study() to reduce the dataset."
      )
    }
  }

  # Ranking of x-axis
  if (sort_by == "performance") {
    method_medians <- tapply(df[[metric]], df$method, median, na.rm = TRUE)
    ordered_methods <- names(sort(method_medians))  # lowest median first
  } else {
    ordered_methods <- sort(unique(df$method))  # alphabetically
  }
  df$method <- factor(df$method, levels = ordered_methods)

  # Base plot
  plot_title <- if (is.null(title)) {
    paste("Boxplots of", metric, "by Method")
  } else if (identical(title, FALSE)) {
    NULL
  } else {
    title
  }

  p <- ggplot2::ggplot(df, aes_args) +
    ggplot2::geom_boxplot(
      outlier.shape = if (show_outliers) 19 else NA,
      notch = notches,
      width = 0.6
    ) +
    ggplot2::labs(
      x = "Method",
      y = metric,
      title = plot_title
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

  # Show mean as a point
  if (show_mean) {
    p <- p + ggplot2::stat_summary(
      fun = mean,
      geom = "point",
      shape = 23,
      size = 2,
      fill = "black"
    )
  }

  # Facet if grouping
  if (!is.null(facet_formula)) {
    p <- p + facet_formula
  }

  # Log scale for Y?
  if (log_scale_y) {
    p <- p + ggplot2::scale_y_log10()
  }

  # Y-limit (coord_cartesian so we don't drop data)
  if (!is.null(y_limit)) {
    p <- p + ggplot2::coord_cartesian(ylim = y_limit)
  }

  # Save or return
  if (!is.null(path)) {
    ggplot2::ggsave(filename = path, plot = p, width = 12, height = 6, units = "in", dpi = 350)
    invisible(p)
  } else {
    return(p)
  }
}

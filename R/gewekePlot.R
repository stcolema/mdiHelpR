#!/usr/bin/env Rscript

#' @title Geweke plot
#' @description Creates a plot of the Geweke Z-score compareing fractions of 
#' samples of the data to the final samples.
#' 
#' @param x An mcmc object.
#' @param frac_1 A fraction between 0 and 1, the fraction of samples to consider 
#' in the first Z-score calculation.
#' @param frac_2 A fraction between 0 and 1, the fraction of samples from the 
#' end of the MCMC iterations to be considered in calculating the Z-score.
#' @param n_bins The number of bins of samples to be considered.
#' @param p_value_threshold The significance threshold included in the final 
#' plot.
#' @param threshold_line_colour The colour of the significance threshold line.
#' @param plt_title The title of the plot.
#' 
#' @return A line graph (ggplot2 object) of the Geweke Z-score at different 
#' intervals.
#' @importFrom coda as.mcmc.list nvar varnames geweke.diag
#' @importFrom ggplot2 ggplot aes geom_line geom_hline ylim facet_wrap labs
#' @importFrom stats start end window qnorm
#' @importFrom tidyr gather_
#' @export
gewekePlot <- function(x,
                       frac_1 = 0.1,
                       frac_2 = 0.5,
                       n_bins = 20,
                       p_value_threshold = 0.05,
                       threshold_line_colour = "grey",
                       plt_title = "Geweke diagnostic plot") {
  
  # The preferred object type for interacting with coda functions
  x <- coda::as.mcmc.list(x)
  
  # The vector of start iterations to calculate the Geweke statistic for
  start_iter_vec <- seq(
    from = stats::start(x),
    to = (stats::start(x) + stats::end(x)) / 2,
    length = n_bins
  )
  
  # The matrix that will hold the Geweke stat
  geweke_mat <- matrix(nrow = length(start_iter_vec), ncol = coda::nvar(x), dimnames = list(start_iter_vec, coda::varnames(x)))
  
  for (n in 1:length(start_iter_vec)) {
    curr_geweke_diag <- coda::geweke.diag(stats::window(x, start = start_iter_vec[n]),
                                    frac1 = frac_1,
                                    frac2 = frac_2
    )
    
    geweke_mat[n, ] <- curr_geweke_diag[[1]]$z
  }
  
  # The 1.96 threshold for 0.05 significance on a standard normal distribution
  c_limit <- stats::qnorm(1 - p_value_threshold / 2)
  
  # The variables to gather when moving from wide to long data (these are our
  # parameters)
  vars_to_gather <- coda::varnames(x)
  
  # The data.frame we will plot (transform to long data to use the ggplot2
  # framework)
  geweke_df <- data.frame(Start_iteration = start_iter_vec) %>%
    cbind(geweke_mat) %>%
    tidyr::gather_("Parameter", "Geweke_statistic", vars_to_gather)
  
  # For this kind of plot I prefer unifrom axis, thus we find the y-axis limits
  y_limit <- max(c(c_limit, abs(geweke_df$Geweke_statistic)))
  
  # Plot the Geweke statistic for each parameter including a significance
  # threshold
  p <- ggplot2::ggplot(geweke_df, ggplot2::aes(x = Start_iteration, y = Geweke_statistic)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = c_limit, linetype = "dashed", color = threshold_line_colour) +
    ggplot2::geom_hline(yintercept = -c_limit, linetype = "dashed", color = threshold_line_colour) +
    ggplot2::ylim(-y_limit, y_limit) +
    ggplot2::facet_wrap(~Parameter) +
    ggplot2::labs(
      x = "First iteration in segment",
      y = "Geweke's convergence dianostic",
      title = plt_title
    )
  
  p
}

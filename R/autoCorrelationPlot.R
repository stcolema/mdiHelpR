#!/usr/bin/env Rscript

#' @title Auto-correlation plot
#' @description Makes an auto-correlation plot as per the coda package, but in
#' the ggplot2 environment rather than base R.
#'
#' @param x An mcmc chain.
#' @param plot_title The plot title.
#' @param lag_max The maximum lag for which to calculate auto-correlation.
#' Defaults to 50.
#' @param facet A logical instructing the plotting of the autocorrelaiton plots
#' of the parameters as facetted upon parameter or else combined in one plot and
#' separated by colour.
#' @return A plot of the auto-correlation for each parameter in x over a number
#' of lag values.
#' @importFrom coda mcmc.list nvar varnames
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs
#' @importFrom stats as.ts acf
#' @importFrom tidyr gather_
#' @export
autoCorrelationPlot <- function(x,
                                plot_title = "autocorrelation",
                                lag_max = NULL,
                                facet = FALSE,
                                threshold_line_colour = "grey") {

  # The preferred object type for interacting with coda functions
  x <- coda::mcmc.list(x)

  # Consider the chain as a time series and calculate the auto-correlation
  x_auto_cor <- x[[1]] |>
    stats::as.ts() |>
    stats::acf(lag.max = lag_max, plot = FALSE)

  # Create a data.frame to hold the autocorrelation info
  auto_cor_df <- data.frame(Lag = x_auto_cor$lag[, 1, 1])

  # Add the autocorrelation for each variable to said data.frame
  for (i in 1:coda::nvar(x)) {
    auto_cor_df <- cbind(auto_cor_df, x_auto_cor$acf[, i, i])
  }

  colnames(auto_cor_df) <- c("Lag", coda::varnames(x))

  # The variables to be considered in going from wide to long
  vars_to_gather <- coda::varnames(x)

  # The long form of the original dataset (to better fit ggplot2)
  auto_cor_df_long <- tidyr::gather_(auto_cor_df,
    "Parameter",
    "Autocorrelation",
    vars_to_gather,
    factor_key = TRUE
  )

  # Create a line plot of the autocorrelation in each parameter
  # Either split parameter's by colour or facet wrap
  if (facet) {
    # Plots!
    p <- ggplot2::ggplot(
      auto_cor_df_long,
      ggplot2::aes(x = Lag, y = Autocorrelation)
    ) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~Parameter) +
      ggplot2::labs(
        title = plot_title
      ) +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = threshold_line_colour
      )
  } else {
    # Plots!
    p <- ggplot2::ggplot(
      auto_cor_df_long,
      ggplot2::aes(x = Lag, y = Autocorrelation, colour = Parameter)
    ) +
      ggplot2::geom_line() +
      ggplot2::labs(
        title = plot_title
      ) +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = threshold_line_colour
      )
  }
  p
}

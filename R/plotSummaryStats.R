#!/usr/bin/env Rscript

#' @title Plot Summary Stats
#' @description Plot the mean against the standard deviation of each feature 
#' with marginal histograms.
#' @param x data.frame of interest
#' @param main title of the plot (default is NULL).
#' @param xlab label for x-axis in plot (default is "Mean")
#' @param ylab label for y-axis in plot (defualt is "Standard deviation")
#' @return A scatter plot of the Mean vs the Standard deviaiton of each feature 
#' of x annotated by marginal histograms. A ggExtra object.
#' @importFrom ggplot2 ggplot aes geom_point labs
#' @importFrom ggExtra ggMarginal
#' @export
plotSummaryStats <- function(x,
                             main = NULL,
                             xlab = "Mean",
                             ylab = "Standard deviation") {
  
  # Create a data.frame of the summary stats
  plt_data <- summaryDF(x)
  
  # Make the scatter plot
  p <- plt_data %>%
    ggplot(aes(x = Mean, y = Std_dev)) +
    geom_point() +
    labs(title = main, x = xlab, y = ylab)
  
  # with marginal histogram
  p1 <- ggMarginal(p, type = "histogram")
  
  
  p1
}
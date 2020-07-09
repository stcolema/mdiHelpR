#!/usr/bin/env Rscript

#' @title Summary data.frame
#' @description Create a data.frame of the means and standard deviations of each
#' feature.
#' @param x data.frame of interest
#' @return A two-column data.frame where the ith row contains the mean and 
#' standard deviation of the ith feature of x.
#' @export
summaryDF <- function(x) {
  
  # Calculate column means and standard deviations
  means <- colMeans(x)
  sds <- apply(x, 2, sd)
  
  # Save to a new data.frame
  new_df <- data.frame(Mean = means, Std_dev = sds)
  new_df
}
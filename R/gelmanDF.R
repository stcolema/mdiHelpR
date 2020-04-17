#!/usr/bin/env Rscript

#' @title Gelman data frame
#' @description Create data suitable for a plot of the Gelman-Rubin shrinkage 
#' factor.
#' @importFrom coda as.mcmc.list varnames
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual facet_wrap labs geom_hline
#' @importFrom tidyr pivot_longer
#' @importFrom tibble add_column
#' @export
gelmanDF <- function(x,
                     max_bins = 50,
                     confidence = 0.95,
                     transform = FALSE,
                     autoburnin = TRUE){
  
  # Convert list of chains to mcmc list for coa and mcclust functions
  x <- coda::as.mcmc.list(x)
  
  # The confidence threshold used
  confidence_threshold <- paste(50 * (confidence + 1), "%", sep = "")
  
  # Claculate the values of the Gelman-Rubin shrinkage factor for the
  # different bins
  gelman_obj <- gelmanValues(x,
                             max_bins = max_bins,
                             confidence = confidence,
                             transform = transform,
                             autoburnin = autoburnin
  )
  
  # Separate out the objects of interest
  shrink <- gelman_obj$shrink
  last_iter_vec <- gelman_obj$last_iter_vec
  
  # Take the median and confidence threshold out
  median_values <- shrink[, , "median"]
  threshold_values <- shrink[, , confidence_threshold]
  
  # The variables in x
  vars_to_gather <- coda::varnames(x)
  
  # If there is only onve variable we have to behave slightly
  # differently as some functions give different names
  if (length(vars_to_gather) == 1) {
    
    # Collect the threshold data and the median data into data.frames
    
    threshold_df <- data.frame(
      "Last_iter" = last_iter_vec,
      "Quantity" = confidence_threshold,
      param = threshold_values
    ) %>%
      magrittr::set_colnames(c(
        "Last_iter",
        "Quantity",
        vars_to_gather
      )) %>%
      tidyr::pivot_longer(-c(Last_iter, Quantity),
                          names_to = "Parameter",
                          values_to = "Shrinkage_factor"
      )
    
    median_df <- data.frame(
      "Last_iter" = last_iter_vec,
      "Quantity" = "median",
      param = median_values
    ) %>%
      magrittr::set_colnames(c(
        "Last_iter",
        "Quantity",
        vars_to_gather
      )) %>%
      tidyr::pivot_longer(-c(Last_iter, Quantity),
                          names_to = "Parameter",
                          values_to = "Shrinkage_factor"
      )
    
  } else {
    
    threshold_df <- data.frame(
      "Last_iter" = last_iter_vec,
      "Quantity" = confidence_threshold
    ) %>%
      cbind(threshold_values) %>%
      tidyr::pivot_longer(-c(Last_iter, Quantity),
                          names_to = "Parameter",
                          values_to = "Shrinkage_factor"
      )
    
    median_df <- data.frame(
      "Last_iter" = last_iter_vec,
      "Quantity" = "median"
    ) %>%
      cbind(median_values) %>%
      tidyr::pivot_longer(-c(Last_iter, Quantity),
                          names_to = "Parameter",
                          values_to = "Shrinkage_factor"
      )
  }
  
  # Bind these
  gelman_df <- rbind(median_df, threshold_df)
  
  gelman_df
  
}
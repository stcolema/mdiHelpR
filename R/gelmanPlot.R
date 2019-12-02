#!/usr/bin/env Rscript

#' @title gelman plot
#' @description Create a plot of the Gelman-Rubin shrinkage factor
#' @importFrom coda as.mcmc.list varnames
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual facet_wrap labs geom_hline
#' @importFrom tidyr gather
#' @export
gelmanPlot <- function(x,
                       max_bins = 50,
                       confidence = 0.95,
                       transform = FALSE,
                       autoburnin = TRUE,
                       auto.layout = TRUE,
                       xlab = "last iteration in chain",
                       ylab = "shrink factor",
                       title = "Gelman-Rubin diagnostic plot") {
  x <- coda::as.mcmc.list(x)
  
  confidence_threshold <- paste(50 * (confidence + 1), "%", sep = "")
  
  gelman_obj <- gelmanValues(x,
                             max_bins = max_bins,
                             confidence = confidence,
                             transform = transform,
                             autoburnin = autoburnin
  )
  
  shrink <- gelman_obj$shrink
  last_iter_vec <- gelman_obj$last_iter_vec
  
  median_values <- shrink[, , "median"]
  threshold_values <- shrink[, , confidence_threshold]
  
  vars_to_gather <- coda::varnames(x)
  
  
  
  threshold_df <- data.frame(
    "Last_iter" = last_iter_vec,
    "Quantity" = confidence_threshold
  ) %>%
    cbind(threshold_values) %>%
    tidyr::gather("Parameter", "Gelman_stat", vars_to_gather)
  
  
  median_df <- data.frame("Last_iter" = last_iter_vec, "Quantity" = "median") %>%
    cbind(median_values) %>%
    tidyr::gather("Parameter", "Gelman_stat", vars_to_gather)
  
  
  gelman_df <- rbind(median_df, threshold_df)
  
  p <- ggplot2::ggplot(gelman_df, ggplot2::aes(x = Last_iter, y = Gelman_stat, colour = Quantity, linetype = Quantity)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = c("black", "grey")) +
    ggplot2::facet_wrap(~Parameter) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title
    ) +
    ggplot2::geom_hline(yintercept = 1L, colour = "red", linetype = 2)
  
  p
}
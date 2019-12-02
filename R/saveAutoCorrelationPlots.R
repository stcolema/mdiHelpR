#!/usr/bin/env Rscript

# Save autocorrelation plots using ggplot2 style
#' @importFrom ggplot2 ggsave
#' @export
saveAutoCorrelationPlots <- function(mcmc_data, save_dirs, plot_titles,
                                     n_seeds = length(mcmc_data),
                                     lag_max = NULL,
                                     gen_auto_corr_title = "Autocorrelation",
                                     plot_type = ".png",
                                     facet = FALSE,
                                     save_plots = FALSE) {
  
  # The filenames and plot titles for the plots
  auto_corr_file_names <- paste0(save_dirs, "/autocorrelation_plot", plot_type)
  auto_corr_titles <- paste0(plot_titles, ": ", gen_auto_corr_title)
  
  p_lst <- vector("list", n_seeds)
  
  for (i in 1:n_seeds) {
    
    
    # Plot title and filename
    curr_file_name <- auto_corr_file_names[i]
    curr_title <- auto_corr_titles[i]
    
    # Create the autocorrelation plot
    p_lst[[i]] <- autoCorrelationPlot(mcmc_data[[i]], curr_title, lag_max = lag_max, facet = facet)
    
    if (save_plots) {
      ggplot2::ggsave(curr_file_name, plot = p_lst[[i]])
    }
  }
  # Return a list of plots
  p_lst
}
#!/usr/bin/env Rscript

#' @importFrom coda mcmc.list as.mcmc
#' @importFrom ggplot2 ggsave
#' @export
saveGewekePlots <- function(mcmc_lst, save_dirs, plot_titles,
                            n_seeds = length(mcmc_lst),
                            gen_title = "Geweke plot",
                            plot_type = ".png",
                            save_plots = FALSE,
                            frac_1 = 0.1,
                            frac_2 = 0.5,
                            n_bins = 20,
                            p_value_threshold = 0.05,
                            threshold_line_colour = "grey") {
  
  
  # The filenames and plot titles for the plots
  file_names <- paste0(save_dirs, "/geweke_plot", plot_type)
  plot_titles <- paste0(plot_titles, ": ", gen_title)
  
  p_lst <- vector("list", n_seeds)
  
  for (i in 1:n_seeds) {
    x <- coda::mcmc.list(coda::as.mcmc(mcmc_lst[[i]]))
    
    # Plot title and filename
    curr_file_name <- file_names[i]
    curr_title <- plot_titles[i]
    
    p_lst[[i]] <- gewekePlot(x,
                             frac_1 = frac_1,
                             frac_2 = frac_2,
                             n_bins = n_bins,
                             p_value_threshold = p_value_threshold,
                             plt_title = curr_title,
                             threshold_line_colour = threshold_line_colour
    )
    
    if (save_plots) {
      ggplot2::ggsave(curr_file_name, plot = p_lst[[i]])
    }
  }
  # Return a list of plots
  p_lst
}

#!/usr/bin/env Rscript

# Save ESS plots
#' @title Make ESS plots
#' @description Makes ESS plots for a list of MCMC chains (can be a list of 1). 
#' These are plots of the effective sample size calculated for different 
#' potential burn-in values and are an aid in choosing an appropriate burn-in
#' value.
#' @param mcmc_lst A list of mcmc chains.
#' @param plot_titles The vector of plot titles corresponding to each chain in
#' mcmc_lst. If NULL defaults to the length of mcmc_lst of the format "chain_i" 
#' @param gen_ess_title The component of the title desired in each plot. 
#' Defaults to "Burn in diagnosis plot". Combined with plot_titles in the form
#' ``paste0(plot_titles[i], ": ", gen_ess_title)``
#' @param save_plots Logical. Instruction to save the plots that are generated.
#' @param save_dirs Characted vector. The locations to save the plots in.
#' @param plot_type The plot type (one of ".png" or ".pdf"). Defaults to ".png".
#' @param common_burn_in An integer. Represents the burn-in the user intends to 
#' apply to each chain. Added as a vertical line to each plot.
#' @return A list of ggplot2 plots.
#' @importFrom coda is.mcmc.list mcmc.list
#' @importFrom ggplot2 labs geom_vline aes scale_linetype_manual guide_legend ggsave
#' @export
makeESSPlots <- function(mcmc_lst, 
                         plot_titles = NULL,
                         gen_ess_title = "Burn in diagnosis plot",
                         save_plots = FALSE,
                         save_dirs = ".",
                         plot_type = ".png",
                         common_burn_in = NULL) {
  
  if(save_plots){
    ess_file_names <- paste0(save_dirs, "/burn_in_plot", plot_type)
  }
  
  if(! coda::is.mcmc.list(mcmc_lst)){
    mcmc_lst <- coda::mcmc.list(mcmc_lst)
  }
  
  # The number of chains in mcmc_lst
  n_chains <- length(mcmc_lst)
  
  # The vector of plot titles
  if(is.null(plot_titles)){
    plot_titles <- paste("chain", 1:n_chains)
  }
  ess_titles <- paste0(plot_titles, ": ", gen_ess_title)
  
  p_lst <- vector("list", n_chains)
  
  for (i in 1:n_chains) {
    
    # Plot title
    curr_title <- ess_titles[i]
    
    # Create plot
    p_lst[[i]] <- plotESSBurn(mcmc_lst[[i]]) +
      ggplot2::labs(
        title = curr_title,
        x = "Tested burn in",
        y = "Effective sample size"
      ) 
    # + theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
    if(! is.null(common_burn_in)){
      p_lst[[i]] <- p_lst[[i]] +
        ggplot2::geom_vline(ggplot2::aes(xintercept= common_burn_in, linetype = "Applied burn-in"), colour= 'red') +
        ggplot2::scale_linetype_manual(name = "", values = c(2), 
                              guide = ggplot2::guide_legend(override.aes = list(color = c("red"))))
    }
    
    # Save
    if (save_plots) {
      # Current save name
      curr_file_name <- ess_file_names[i]
      ggplot2::ggsave(curr_file_name, plot = p_lst[[i]])
    }
  }
  p_lst
}

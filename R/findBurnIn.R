#!/usr/bin/env Rscript

# Find the appropriate burn-in for a given chain of MCMC (approximate and
# possible to fail - check plottd output to be sure)
#' @title Find burn-in
#' @description Given a chains of MCMC output, this function finds an
#' appropriate burn-in across for the chain. It does this by calculating the 
#' effective smaple size (ESS) for each variable at a number of potential 
#' burn-in values and then selecting the burn-in value for which the sum of
#' these is maximised. This method is somewhat approximate (as it assumes a 
#' common order of magnitude for each ESS time series) so we reccomend combining
#' this with makeESSPlots to check if it makes sense.
#' @param mcmc_data A mcmc object (as per the coda package).
#' @param max_burn_in The maximum burn-in value to test against. Defaults to 
#' half the number of samples present.
#' @param step_size The granularity of burn-in values to test (i.e. the distance 
#' between subsequent values). Defaults to 0.02 of the max_burn_in.
#' 
#' @return An integer. The reccomended burn in for the current chain.
#' 
#' @example
#' findBurnIn(mcmc_data)
#' 
#' @example 
#' # applied across multiple chains: 
#' sapply(mcmc_lst, findBurnIn)
#' @importFrom coda varnames
#' @importFrom plyr ldply
#' @export
findBurnIn <- function(mcmc_data,
                       max_burn_in = ifelse(is.data.frame(mcmc_data) | coda::is.mcmc(mcmc_data), 
                                            nrow(mcmc_data), 
                                            nrow(mcmc_data[[1]])) / 2,
                       step_size = round(max_burn_in / 50)) {
  
  
  # What parameters are present
  # all_parameters <- createParameterNames(n_datasets)
  all_parameters <- coda::varnames(mcmc_data)
  
  # The function describing the sum of all parameters ESS
  param_func <- paste(all_parameters, collapse = "+")
  
  
  # Vector of values to test effective sample size (ESS) at
  test_burn_in <- seq(0, max_burn_in, step_size) # test values
  
  # If not already a list, change mcmc_out to a list
  if (!class(mcmc_data) %in% c("mcmc.list", "list")) {
    mcmc_data <- list("chain_1" = mcmc_data)
  }
  
  # If there are no names, set these to chain_{1..n_chains}
  if (is.null(names(mcmc_data))) {
    names(mcmc_data) <- paste0("chain_", seq_along(mcmc_data))
  }
  
  # Create a data.frame of effective sample size for each parameter at each
  # possible burn in
  df_ess.burn.in <- plyr::ldply(mcmc_data, .fun = findESS, test_burn_in)
  
  # Create a variable comparing the ESS for each variable
  est_burn_in_df <- df_ess.burn.in %>%
    stringMutate(param_func, "Total")
  
  # Choose the burn in for which the parameters have the largest combined ESS
  est_burn_in <- est_burn_in_df$burn_in[est_burn_in_df$Total == max(est_burn_in_df$Total)]
  
  est_burn_in
}
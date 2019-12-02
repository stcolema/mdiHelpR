#!/usr/bin/env Rscript

# Calculate the expected sample size for a chain for a given test burn-in
#' @title find ESS
#' @description Calculates the effective sample size (ESS) for an MCMC chain 
#' over a range of potential burn-in values.
#' @param mcmc_out An MCMC object (as per the coda package)
#' @param test_burn_in A numrical vector of different burn in values to 
#' calculate the ESS at.
#' @return A data.frame of the ESS for different values of burn-in.
#' @importFrom coda effectiveSize as.mcmc
#' @export
findESS <- function(mcmc_out, test_burn_in) {
  
  # initialise data.frame of ess estimates
  ess_burn_in <- data.frame(t(coda::effectiveSize(mcmc_out)))
  
  # loop over all test values after 0
  for (burn_in in test_burn_in[-1]) {
    
    # test burn-in
    test.trace <- burnAndThin(mcmc_out, burn = burn_in)
    
    # estimate ESS and at to vector of ess estimates
    ess_burn_in <- rbind(ess_burn_in, t(coda::effectiveSize(coda::as.mcmc(test.trace))))
  }
  
  ess_burn_in$burn_in <- test_burn_in
  
  ess_burn_in
}

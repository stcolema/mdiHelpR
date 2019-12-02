#!/usr/bin/env Rscript

# Create parameter names for output from MDI for a given number of datasets
#' @title Create parameter names 
#' @description Creates parameter names from an MDI analysis based upon the
#' number of datasets used.
#' 
#' @param n_datasets Integer. The number of datasets used in the analysis.
#' @return A vector of strings. There will be n_datasets MassParameter_ 
#' variables (appended 1:n_datasets) and n_datasets choose 2 Phi_ parameters
#' (appended by ij, where i and j are dataset indices)
#' @export
createParameterNames <- function(n_datasets) {
  # The parameters of interest
  
  # The mass parameters - 1 per dataset
  mass_param_names <- paste0("MassParameter_", 1:n_datasets)
  
  # The phi parameters are combinatorical
  phi_indices <- combn(1:n_datasets, 2) %>%
    apply(., 2, paste0, collapse = "")
  
  phi_param_names <- paste0("Phi_", phi_indices)
  
  all_parameters <- c(mass_param_names, phi_param_names)
  
  all_parameters
}
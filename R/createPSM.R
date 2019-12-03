#!/usr/bin/env Rscript

# Wrapper function to find the PSM for a data.table
#' @title Make PSM
#' @export
makePSM <- function(my_dt) {
  
  n <- nrow(my_dt)
  p <- ncol(my_dt)
  
  list_sim <- calcPSMEntries(my_dt, n, p)
  psm <- constructPSM(list_sim, p)
  
  constructSymmetrix(psm, p)
}
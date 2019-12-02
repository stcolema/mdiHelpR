#!/usr/bin/env Rscript

#' @title Gelman values
#' @importFrom coda niter thin nvar varnames gelman.diag
#' @importFrom stats start end window
#' @export
gelmanValues <- function(x,
                         max_bins = 50,
                         confidence = 0.95,
                         transform = FALSE,
                         autoburnin = TRUE) {
  n_bins <- min(floor((coda::niter(x) - 50) / coda::thin(x)), max_bins)
  
  if (n_bins < 1) {
    stop("Insufficient iterations to produce Gelman-Rubin plot")
  }
  
  # Set the bin width within which to calculate the Shrinkage factor
  bin_width <- floor((coda::niter(x) - 50) / n_bins)
  
  # The last iteration considered within each bin
  last_iter_vec <- c(
    seq(
      from = stats::start(x) + 50 * coda::thin(x),
      by = bin_width * coda::thin(x),
      length = n_bins
    ),
    stats::end(x)
  )
  
  # String for the 1-way confidence interval
  confidence_threshold <- paste(50 * (confidence + 1), "%", sep = "")
  
  # Array to hold the shrinkage factor (median and condifence threshold)
  shrink <- array(dim = c(n_bins + 1, coda::nvar(x), 2))
  
  dimnames(shrink) <- list(
    last_iter_vec, coda::varnames(x),
    c("median", confidence_threshold)
  )
  
  # Calculate the shrinkage factor for each bin of iterations
  for (i in 1:(n_bins + 1)) {
    shrink[i, , ] <- coda::gelman.diag(stats::window(x, end = last_iter_vec[i]),
                                 confidence = confidence,
                                 transform = transform,
                                 autoburnin = autoburnin,
                                 multivariate = FALSE
    )$psrf
  }
  
  all.na <- apply(
    is.na(shrink[, , 1, drop = FALSE]), 2,
    all
  )
  if (any(all.na)) {
    cat("Cannot compute Gelman & Rubin's diagnostic for any chain \n")
    cat("segments for variables", coda::varnames(x)[all.na], "\n")
    cat("This indicates convergence failure\n")
    stop()
  }
  
  list("shrink" = shrink, "last_iter_vec" = last_iter_vec)
}

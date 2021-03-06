% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeESSPlots.R
\name{makeESSPlots}
\alias{makeESSPlots}
\title{Make ESS plots}
\usage{
makeESSPlots(
  mcmc_lst,
  plot_titles = NULL,
  gen_ess_title = "Burn in diagnosis plot",
  save_plots = FALSE,
  save_dirs = ".",
  plot_type = ".png",
  common_burn_in = NULL
)
}
\arguments{
\item{mcmc_lst}{A list of mcmc chains.}

\item{plot_titles}{The vector of plot titles corresponding to each chain in
mcmc_lst. If NULL defaults to the length of mcmc_lst of the format "chain_i"}

\item{gen_ess_title}{The component of the title desired in each plot. 
Defaults to "Burn in diagnosis plot". Combined with plot_titles in the form
``paste0(plot_titles[i], ": ", gen_ess_title)``}

\item{save_plots}{Logical. Instruction to save the plots that are generated.}

\item{save_dirs}{Characted vector. The locations to save the plots in.}

\item{plot_type}{The plot type (one of ".png" or ".pdf"). Defaults to ".png".}

\item{common_burn_in}{An integer. Represents the burn-in the user intends to 
apply to each chain. Added as a vertical line to each plot.}
}
\value{
A list of ggplot2 plots.
}
\description{
Makes ESS plots for a list of MCMC chains (can be a list of 1). 
These are plots of the effective sample size calculated for different 
potential burn-in values and are an aid in choosing an appropriate burn-in
value.
}

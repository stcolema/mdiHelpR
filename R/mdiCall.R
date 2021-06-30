#!/usr/bin/Rscript

# Call MDI++ from within R
# Example
# Rscript callMDIpp.R --datasets "~/rds/hpc-work/Input_data/Yeast_data/Reduced/Granovskaia_timecourse_normalised_reduced.csv ~/rds/hpc-work/Input_data/Yeast_data/Reduced/harbison_marina.csv ~/rds/hpc-work/Input_data/Yeast_data/Reduced/ppi.csv" --datatypes "GP M M" --n_iter 1000 --mdi_dir "/home/sdc56/hpc_stuff/scripts/mdipp-1.0.1/" --thin 1 -k 50 -s 1
#
# Stephen Coleman
# 15/07/2020

#' @title Run MDI
#' @description Calls mdi++.cpp (Mason et al., 2016), the C++ implementation of
#' Multiple Dataset Integration (Kirk et al., 2012).
#' @param datasets A list of files as inputs to mdi++
#' @param dataTypes A list of data types for the dataset specific mixutre models.
#' Entries are paired with datasets and the orders should match.
#' @param mdipp_dir The directory mdi++.cpp lives in. If mdi is saved to the
#' PATH, then use the default of ``NULL``.
#' @param K The maximum number of clusters across all datasets in MDI.
#' @param thin The thinning factor for the MCMC samples.
#' @param seed The random seed controlling the algorithm.
#' @param output The file name to save the output too; defaults to the current
#' directory and a string capturing the datasets and parameters used.
#' @param call Logical indicating to call the generated command.
#' @return A string expressing the appropriate command line call for MDI.
#' @importFrom stringr str_to_lower str_remove
#' @export
runMDI <- function(datasets, dataTypes, R,
                   mdipp_dir = NULL,
                   K = 50,
                   thin = 1,
                   seed = 1,
                   output = NULL,
                   call = TRUE) {
  L <- length(datasets)

  if (L != length(dataTypes)) {
    stop("Number of datasets and number of data types not equal.")
  }

  # options <- sprintf("-n ${num_iter} -t ${thin} -s ${seed} -c ${n_clust} > ${output}")
  options <- sprintf(" -n %s -t %s -s %s -c %s", R, thin, seed, K)


  cmd <- paste0(mdipp_dir, "mdipp")

  data_str <- ""
  for (l in 1:L) {
    curr_type <- stringr::str_to_lower(dataTypes[l])
    mdippType <- NULL
    if (curr_type %in% c("gaussian", "normal", "g", "n")) {
      mdippType <- "N"
    }
    if (curr_type %in% c("categorical", "multinomial", "c", "m")) {
      mdippType <- "M"
    }
    if (curr_type %in% c("gaussianprocess", "gp")) {
      mdippType <- "GP"
    }
    if (curr_type %in% c("bagofwords", "bw")) {
      mdippType <- "BW"
    }
    if (is.null(mdippType)) {
      stop(cat("Data type ", l, " is not acceptable. Please check inputs.\n"))
    }

    data_str <- paste(data_str, mdippType, datasets[l])
  }

  cmd <- paste0(cmd, data_str, options)

  if (is.null(output)) {
    if (L < 5) {
      filenames <- datasets %>%
        strsplit("/") %>%
        lapply(function(x) x[length(x)]) %>%
        stringr::str_remove(".csv") %>%
        unlist()

      output <- paste0(filenames, collapse = "")
    } else {
      today <- strsplit(date(), " ")
      output <- paste0(today[[1]][3], today[[1]][2], today[[1]][5])
    }

    output <- paste0(output, "R", R, "T", thin, "K", K, "S", seed, ".csv")
  }

  cmd <- paste0(cmd, " > ", output)

  if (call) system(cmd)

  cmd
}

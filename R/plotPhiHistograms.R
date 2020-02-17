#!/usr/bin/env Rscript

# Function to save histogram of phi parameter from MDI over MCMC iterations

#' @importFrom ggplot2 ggplot aes_string geom_histogram labs ggsave
#' @importFrom readr parse_number
#' @export
plotPhiHistograms <- function(phis, file_path, start_index, eff_n_iter,
                              burn = 0,
                              thin = 1) {
  loc_dir <- paste0(file_path, "Phi_histograms/")
  dir.create(loc_dir, showWarnings = FALSE)

  for (i in 1:length(phis)) {
    for (j in 1:ncol(phis[[i]])) {
      curr_phi <- colnames(phis[[i]])[j]
      plot_name <- paste0(loc_dir, curr_phi, "_plot", plot_type)

      # Find which datasets are used here
      dataset_indices <- readr::parse_number(curr_phi)

      # The following steps do not work if we have more than 9 datasets (which is not relevant to me)
      # This extracts the first dataset index (i.e. from 67 takes 6)
      dataset_index_1 <- dataset_indices[1] / 10 %>%
        floor(.)

      # This extracts the secodn dataset index
      dataset_index_2 <- dataset_indices[1] %% 10

      dataset_1 <- dataset_names[dataset_index_1]
      dataset_2 <- dataset_names[dataset_index_2]

      histogram_plot_name <- paste0(loc_dir, curr_phi, "_histogram_plot", plot_type)

      histogram_title <- paste(curr_phi, ": histogram plot")

      histogram_subtitle <- paste(
        "Iterations",
        (burn + thin),
        "through",
        n_iter,
        "for",
        dataset_1,
        "and",
        dataset_2
      )

      # As R does not maintain type when subsetting a data.frame of width 1
      if (ncol(phis[[i]]) == 1) {
        ggplot2::ggplot(
          data = data.frame(Phi_12 = phis[[i]][start_index:eff_n_iter, ]),
          ggplot2::aes_string(x = curr_phi)
        ) +
          ggplot2::geom_histogram() +
          ggplot2::labs(
            title = histogram_title,
            subtitle = histogram_subtitle
          )
      } else {
        ggplot2::ggplot(
          data = phis[[i]][start_index:eff_n_iter, ],
          ggplot2::aes_string(x = curr_phi)
        ) +
          ggplot2::geom_histogram() +
          ggplot2::labs(
            title = histogram_title,
            subtitle = histogram_subtitle
          )
      }

      ggplot2::ggsave(histogram_plot_name)
    }
  }
}

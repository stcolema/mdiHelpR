#!/usr/bin/env Rscript

# Function to save plot of phi parameter from MDI from each iteration of MCMC

#' @importFrom dplyr select contains
#' @importFrom ggplot2 ggplot aes geom_point labs ggsave
#' @importFrom magrittr set_colnames set_rownames
#' @importFrom pheatmap pheatmap
#' @export
plotPhiSeries <- function(mcmc_out_lst,
                          file_path,
                          num_files,
                          num_datasets,
                          start_index,
                          eff_n_iter,
                          save_plots = F,
                          col_pal = grDevices::colorRampPalette(c("white", "#146EB4"))(100),
                          burn = 0,
                          thin = 1) {
  phis <- list()
  count <- 0

  # print("Saving plots of phi as a time series.")

  # Create directory to save this output in
  loc_dir <- paste0(file_path, "Phi_series_plots/")
  dir.create(loc_dir, showWarnings = FALSE)

  # Iterate over the files and then the combinations of phis
  for (i in 1:num_files) {

    # For heatmapping the phis across datasets
    phi_comparison_df <- as.data.frame(matrix(
      nrow = num_datasets,
      ncol = num_datasets,
      0
    )) %>%
      magrittr::set_colnames(dataset_names) %>%
      magrittr::set_rownames(dataset_names)

    for (j in 1:(num_datasets - 1)) {
      for (k in (j + 1):num_datasets) {

        # Count for the index of the list object where phis are stored
        count <- count + 1

        col_name <- paste0("Phi_", j, k)

        # Pull out the column for the relevant phi
        phis[[count]] <- mcmc_out_lst[[i]] %>%
          dplyr::select(dplyr::contains(col_name))

        # Which tissues
        dataset_j <- dataset_names[[j]]
        dataset_k <- dataset_names[[k]]

        # Create plot labels
        plot_title <- bquote(Phi ~ "for" ~ .(dataset_j) ~ "and" ~ .(dataset_k))
        y_axis_title <- substitute(Phi[ind1], list(ind1 = paste0(j, k)))
        sub_title <- paste("Iterations", (burn + thin), "through", n_iter)

        # The save file name
        save_title <- paste0(loc_dir, "file_", i, "_Phi_", j, k, plot_type)

        # Put things in a data frame to use ggplot
        my_data_frame <- data.frame(
          Index = start_index:eff_n_iter * thin,
          Phi = phis[[count]][[1]][start_index:eff_n_iter]
        )

        # Plot
        ggplot2::ggplot(data = my_data_frame, ggplot2::aes(x = Index, y = Phi)) +
          ggplot2::geom_point() +
          ggplot2::labs(
            title = plot_title,
            # subtitle = sub_title,
            y = y_axis_title,
            x = "Iteration"
          )

        # Save
        ggplot2::ggsave(save_title)

        mean_phi <- phis[[count]][[1]][start_index:eff_n_iter] %>% mean()

        phi_comparison_df[j, k] <- phi_comparison_df[k, j] <- mean_phi
      }
    }

    # Heatmap the average phi (after some burn in)
    phi_pheatmap_title <- "Heatmap comparing phis across datasets"
    phi_pheatmap_file_name <- paste0(save_path, "Phi_heatmap_", i, plot_type)

    if (save_plots) {
      pheatmap::pheatmap(phi_comparison_df,
        main = phi_pheatmap_title,
        cluster_rows = F,
        cluster_cols = F,
        filename = phi_pheatmap_file_name,
        color = col_pal
      )

      # dev.off()
    } else {
      pheatmap::pheatmap(phi_comparison_df,
        main = phi_pheatmap_title,
        cluster_rows = F,
        cluster_cols = F,
        color = col_pal
        # filename = phi_pheatmap_file_name
      )
    }
  }
  # phis
}

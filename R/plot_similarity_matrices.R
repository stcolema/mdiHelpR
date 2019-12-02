#!/usr/bin/env Rscript

# Function to save heatmap of PSMs

#' @importFrom pheatmap pheatmap
#' @export
plot_similarity_matrices <- function(similarity_matrices_lst,
                                     # probes_present_final,
                                     dataset_names,
                                     n_files,
                                     n_datasets,
                                     file_path,
                                     col_pal = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 7, name = "Blues"))(100),
                                     breaks = NA,
                                     show_labels = TRUE) {
  loc_dir <- paste0(file_path, "Similarity_matrices/")
  dir.create(loc_dir, showWarnings = FALSE)

  # Define the type of file to save
  # plot_type <- ".pdf" # ".png" or ".pdf"

  for (j in 1:n_files) {
    # curr_save_path <- paste0(save_path, "seed_", j)
    # Iterate over datasets
    for (i in 1:n_datasets) {
      # Find the dataset name
      dataset <- dataset_names[[i]]

      # Create the save location and file name
      file_name <- paste0(loc_dir, "similarity_matrix_", dataset, plot_type)

      # Create the title of the plot
      title <- paste("Similarity matrix for", dataset)

      # rel_rows <- probes_present_final[, i]

      # Make the heatmap (note that we do not cluster rows).
      # We use a sample of the iterations as otherwise it becomes too heavy

      if (save_plots) {
        ph <- pheatmap::pheatmap(similarity_matrices_lst[i + (j - 1) * n_files][[1]],
          main = title,
          cluster_rows = T,
          filename = file_name,
          color =  col_pal,
          breaks = breaks,
          show_rownames = show_labels,
          show_colnames = show_labels
        )
      } else {
        ph <- pheatmap::pheatmap(similarity_matrices_lst[i + (j - 1) * n_files][[1]],
          main = title,
          cluster_rows = T,
          color =  col_pal,
          breaks = breaks,
          show_rownames = show_labels,
          show_colnames = show_labels
        )
      }
    }
  }
  # ph
}

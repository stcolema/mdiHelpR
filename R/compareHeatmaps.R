#!/usr/bin/env Rscript

#' @title Compare heatmaps
#' @importFrom pheatmap pheatmap
#' @export
compareHeatmaps <- function(...,
                               save_name = "joint_heatmaps.png",
                               main = "heatmap_comparison",
                               col_pal = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 7, name = "Blues"))(100)) {
  
  # Contain ellipsis (...) in a list and find its length
  data_to_compare <- list(...)
  n_df <- length(data_to_compare)
  ph_list <- list()
  
  # Create a heatmap of the data without clustering
  for (i in 1:n_df) {
    ph_list[[i]] <- pheatmap::pheatmap(data_to_compare[[i]],
                                       cluster_rows = F,
                                       cluster_cols = F,
                                       color = col_pal,
                                       silent = TRUE
    )$gtable
  }
  
  # Combine these in a grid and save
  combinePheatmaps(ph_list, save_name = save_name, main = main)
}
#!/usr/bin/env Rscript

#' @title Plot comparison expression to clustering
#' @importFrom pheatmap pheatmap
#' @export
plotComparisonData2Clustering <- function(compare_tibble,
                                          dataset_names,
                                          num_datasets,
                                          file_path,
                                          file_type,
                                          gen_title = "Comparison of clustering and gene expression data",
                                          col_pal_sim = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 7, name = "Blues"))(100),
                                          col_pal_expr = grDevices::colorRampPalette(c("#146EB4", "white", "#FF9900"))(100),
                                          sim_breaks = NULL,
                                          expr_breaks = NULL,
                                          show_row_labels = FALSE) {
  
  
  if (is.null(expr_breaks)) {
    expr_breaks <- defineBreaks(col_pal_expr)
  }
  
  if (is.null(sim_breaks)){
    sim_breaks <- defineBreaks(col_pal_sim)
  }
  
  # The directory we will save the plots to
  loc_dir <- paste0(file_path, "Comparison_expression_clustering/")
  
  # Create the directroy if it's does not already exist
  dir.create(loc_dir, showWarnings = FALSE)
  
  # Loop over the datasets creating a grid of the heatmap of the similarity
  # matrix beside that of the expression data in the same row order
  for (i in 1:num_datasets) {
    
    # Find the name of the current dataset
    curr_dataset <- dataset_names[[i]]
    
    # Add this to the plot title
    curr_title <- paste0(curr_dataset, ": ", gen_title)
    
    # The name under which the plot will be saved for this dataset
    comp_plot_name <- paste0(loc_dir, curr_dataset, file_type)
    
    # Extract the relevant parts of the tibble
    curr_sim <- compare_tibble$similarity_matrix[compare_tibble$dataset == curr_dataset][[1]]
    curr_expr <- compare_tibble$expression_data[compare_tibble$dataset == curr_dataset][[1]]
    
    # This is an inefficient method as we are only interested in the clustering
    ph_sim <- pheatmap::pheatmap(curr_sim)
    
    # Extract the order from the pheatmap
    row_order <- ph_sim$tree_row$order
    
    # Find the column order for the expression data
    ph_expr <- pheatmap::pheatmap(curr_expr, cluster_rows = F)
    expr_col_order <- ph_expr$tree_col$order
    
    # Re order the matrices to have a common row order
    curr_sim <- curr_sim[row_order, row_order]
    curr_expr <- curr_expr[row_order, expr_col_order]
    
    colnames(curr_sim) <- NULL
    colnames(curr_expr) <- NULL
    
    # Save the heatmaps of these to the same grid
    compareHeatmapPSMandData(curr_sim, curr_expr,
                             save_name = comp_plot_name,
                             main = curr_title,
                             col_pal_sim = col_pal_sim,
                             col_pal_expr = col_pal_expr,
                             expr_breaks = expr_breaks,
                             sim_breaks = sim_breaks
    )
  }
}
#!/usr/bin/env Rscript

#' @title Plot comparison correlation PSM data
#' @export
plotComparisonCorrelationPSMData <- function(compare_tibble,
                                             dataset_names,
                                             num_datasets,
                                             file_path,
                                             file_type,
                                             gen_title = "Comparison of clustering, gene expression data and data correlation",
                                             col_pal_sim = grDevices::colorRampPalette(c("#FF9900", "white", "#146EB4"))(100),
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
  loc_dir <- paste0(file_path, "Comparison_expression_clustering_correlation/")
  
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
    curr_corr <- compare_tibble$correlation_matrix[compare_tibble$dataset == curr_dataset][[1]] 
    # curr_corr <- curr_expr %>%
    #   t() %>%
    #   cor()
    
    heatmap_wrapper_sim_expr_corr(curr_sim,
                                  curr_expr,
                                  curr_corr,
                                  ph_title = curr_title,
                                  save_name = comp_plot_name,
                                  col_pal_sim = col_pal_sim,
                                  col_pal_expr = col_pal_expr
    )
  }
}



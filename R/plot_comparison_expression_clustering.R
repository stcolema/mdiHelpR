#!/usr/bin/env Rscript

# Functions to save comparison of heatmaps

define_breaks <- function(col_pal, lb = -1, ub = 1){
  palette_length <- length(col_pal)
  
  breaks <- c(
    seq(lb, 0, length.out = ceiling(palette_length / 2) + 1),
    seq(1 / palette_length, ub, length.out = floor(palette_length / 2))
  )
}


#' @importFrom cowplot plot_grid draw_label
#' @importFrom ggplot2 ggsave
#' @export
combine_pheatmaps <- function(ph_list,
                              save_name = "joint_heatmaps.png",
                              main = "heatmap_comparison",
                              font_size = 20) {

  # Find the number of heatmaps to be combined
  n_ph <- length(ph_list)

  # Stop the script if the number of heatmaps is not 2 or 3
  if (n_ph != 2 && n_ph != 3) {
    stop("Currently function only works for 2 or 3 heatmaps.")
  }

  # Use cowplot::plot_grid to put the heatmaps on the same grid and add a common
  # title
  if (n_ph == 2) {
    out_plot <- cowplot::plot_grid(
      NULL, NULL, ph_list[[1]], ph_list[[2]],
      nrow = 2,
      rel_heights = c(0.125, 1)
    ) +
      cowplot::draw_label(main,
        x = 0.5,
        y = 0.95,
        size = font_size,
        fontface = "bold"
      )

    if (is.null(save_name)) {
      return(out_plot)
    }
    if(! is.na(save_name)){
      # Save using ggplot2::ggsave
      ggplot2::ggsave(save_name, width = 12, height = 8)
    }
  }
  if (n_ph == 3) {
    out_plot <- cowplot::plot_grid(
      NULL, NULL, NULL, ph_list[[1]], ph_list[[2]], ph_list[[3]],
      nrow = 2,
      rel_heights = c(0.125, 1)
    ) +
      cowplot::draw_label(main,
        x = 0.5,
        y = 0.95,
        size = font_size,
        fontface = "bold"
      )
    if (is.null(save_name)) {
      return(out_plot)
    }
    if(! is.na(save_name)){
      # Save using ggplot2::ggsave
      ggplot2::ggsave(save_name, width = 18, height = 8)
    }
  }
  out_plot
}

#' @importFrom pheatmap pheatmap
#' @export
heatmap_comparison_sim_expr <- function(...,
                                        save_name = "joint_heatmaps.png",
                                        main = "heatmap_comparison",
                                        col_pal_sim = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 7, name = "Blues"))(100),
                                        col_pal_expr = grDevices::colorRampPalette(c("#146EB4", "white", "#FF9900"))(100),
                                        expr_breaks = NULL,
                                        sim_breaks = NULL,
                                        show_row_labels = FALSE,
                                        show_col_labels = FALSE) {
  
  if (is.null(expr_breaks)) {
    expr_breaks <- define_breaks(col_pal_expr)
    
    # palette_length_expr <- length(col_pal_expr)
    # 
    # expr_breaks <- c(
    #   seq(-1, 0, length.out = ceiling(palette_length_expr / 2) + 1),
    #   seq(1 / palette_length_expr, 1, length.out = floor(palette_length_expr / 2))
    # )
  }
  
  if (is.null(sim_breaks)){
    sim_breaks <- define_breaks(col_pal_sim)
    
    # palette_length_sim <- length(col_pal_sim)
    # 
    # sim_breaks <- c(
    #   seq(-1, 0, length.out = ceiling(palette_length_expr / 2) + 1),
    #   seq(1 / palette_length_expr, 1, length.out = floor(palette_length_expr / 2))
    # )
  }

  # Contain ellipsis (...) in a list and find its length
  data_to_compare <- list(...)
  n_df <- length(data_to_compare)
  ph_list <- list()

  # Create a heatmap of the data without clustering
  ph_list[[1]] <- pheatmap::pheatmap(data_to_compare[[1]],
    cluster_rows = F,
    cluster_cols = F,
    color = col_pal_sim,
    breaks = sim_breaks,
    show_rownames = show_row_labels,
    show_colnames = show_col_labels,
    silent = TRUE
  )$gtable

  ph_list[[2]] <- pheatmap::pheatmap(data_to_compare[[2]],
    cluster_rows = F,
    cluster_cols = F,
    color = col_pal_expr,
    breaks = expr_breaks,
    show_rownames = show_row_labels,
    show_colnames = show_col_labels,
    silent = TRUE
  )$gtable

  # Combine these in a grid and save
  combine_pheatmaps(ph_list, save_name = save_name, main = main)
}


#' @importFrom pheatmap pheatmap
#' @export
heatmap_comparison <- function(...,
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
  combine_pheatmaps(ph_list, save_name = save_name, main = main)
}

#' @importFrom pheatmap pheatmap
#' @export
plot_comparison_expression_to_clustering <- function(compare_tibble,
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
    expr_breaks <- define_breaks(col_pal_expr)
    
    # palette_length_expr <- length(col_pal_expr)
    # 
    # expr_breaks <- c(
    #   seq(-1, 0, length.out = ceiling(palette_length_expr / 2) + 1),
    #   seq(1 / palette_length_expr, 1, length.out = floor(palette_length_expr / 2))
    # )
  }
  
  if (is.null(sim_breaks)){
    sim_breaks <- define_breaks(col_pal_sim)
    
    # palette_length_sim <- length(col_pal_sim)
    # 
    # sim_breaks <- c(
    #   seq(-1, 0, length.out = ceiling(palette_length_expr / 2) + 1),
    #   seq(1 / palette_length_expr, 1, length.out = floor(palette_length_expr / 2))
    # )
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
    heatmap_comparison_sim_expr(curr_sim, curr_expr,
      save_name = comp_plot_name,
      main = curr_title,
      col_pal_sim = col_pal_sim,
      col_pal_expr = col_pal_expr,
      expr_breaks = expr_breaks,
      sim_breaks = sim_breaks
    )
  }
}

#' @export
plot_comparison_corr_sim_expr <- function(compare_tibble,
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
    expr_breaks <- define_breaks(col_pal_expr)
  }
  
  if (is.null(sim_breaks)){
    sim_breaks <- define_breaks(col_pal_sim)
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



#' @importFrom pheatmap pheatmap
#' @export
heatmap_wrapper_sim_expr_corr <- function(sim_mat,
                                          expr_mat,
                                          corr_mat,
                                          ph_title = "Comparison clustering, expression and correlation",
                                          save_name = NULL,
                                          col_pal_sim = grDevices::colorRampPalette(c("#FF9900", "white", "#146EB4"))(100),
                                          col_pal_expr = grDevices::colorRampPalette(c("#146EB4", "white", "#FF9900"))(100),
                                          expr_breaks = NULL,
                                          sim_breaks = NULL,
                                          font_size = 20,
                                          expr_col_order = TRUE,
                                          show_row_labels = FALSE) {
  if (is.null(expr_breaks)) {
    expr_breaks <- define_breaks(col_pal_expr)
    
    # palette_length_expr <- length(col_pal_expr)
    # 
    # expr_breaks <- c(
    #   seq(-1, 0, length.out = ceiling(palette_length_expr / 2) + 1),
    #   seq(1 / palette_length_expr, 1, length.out = floor(palette_length_expr / 2))
    # )
  }
  
  if (is.null(sim_breaks)){
    sim_breaks <- define_breaks(col_pal_sim)
    
    # palette_length_sim <- length(col_pal_sim)
    # 
    # sim_breaks <- c(
    #   seq(-1, 0, length.out = ceiling(palette_length_expr / 2) + 1),
    #   seq(1 / palette_length_expr, 1, length.out = floor(palette_length_expr / 2))
    # )
  }

  # This is an inefficient method as we are only interested in the clustering
  ph_sim <- pheatmap::pheatmap(sim_mat)

  # Extract the order from the pheatmap
  row_order <- ph_sim$tree_row$order

  if(expr_col_order){
  # Extract column order for expression data
  ph_expr <- pheatmap::pheatmap(expr_mat, cluster_rows = F)
  expr_col_order <- ph_expr$tree_col$order
  } else {
    expr_col_order <- ph_sim$tree_col$order
  }
  
  # Re order the matrices to have a common row order
  sim_mat <- sim_mat[row_order, row_order]
  expr_mat <- expr_mat[row_order, expr_col_order]
  corr_mat <- corr_mat[row_order, row_order]

  colnames(sim_mat) <- NULL
  colnames(expr_mat) <- NULL
  colnames(corr_mat) <- NULL

  # Save the heatmaps of these to the same grid
  heatmap_comparison_sim_expr_cor(sim_mat, expr_mat, corr_mat,
    save_name = save_name,
    main = ph_title,
    col_pal_sim = col_pal_sim,
    col_pal_expr = col_pal_expr,
    expr_breaks = expr_breaks,
    sim_breaks = sim_breaks,
    font_size = font_size,
    show_row_labels = show_row_labels
    
  )
}







#' @importFrom pheatmap pheatmap
#' @export
heatmap_comparison_sim_expr_cor <- function(...,
                                            save_name = "joint_heatmaps.png",
                                            main = "heatmap_comparison",
                                            col_pal_sim = grDevices::colorRampPalette(c("#FF9900", "white", "#146EB4"))(100),
                                            col_pal_expr = grDevices::colorRampPalette(c("#146EB4", "white", "#FF9900"))(100),
                                            expr_breaks = NULL,
                                            sim_breaks = NULL,
                                            font_size = 20,
                                            show_row_labels = FALSE,
                                            show_col_labels = FALSE) {
  if (is.null(expr_breaks)) {
    expr_breaks <- define_breaks(col_pal_expr)
    
    # palette_length_expr <- length(col_pal_expr)
    # 
    # expr_breaks <- c(
    #   seq(-1, 0, length.out = ceiling(palette_length_expr / 2) + 1),
    #   seq(1 / palette_length_expr, 1, length.out = floor(palette_length_expr / 2))
    # )
  }
  
  if (is.null(sim_breaks)){
    sim_breaks <- define_breaks(col_pal_sim)
    
    # palette_length_sim <- length(col_pal_sim)
    # 
    # sim_breaks <- c(
    #   seq(-1, 0, length.out = ceiling(palette_length_expr / 2) + 1),
    #   seq(1 / palette_length_expr, 1, length.out = floor(palette_length_expr / 2))
    # )
  }

  # Contain ellipsis (...) in a list and find its length
  data_to_compare <- list(...)
  n_df <- length(data_to_compare)
  ph_list <- list()

  # Create a heatmap of the data without clustering
  ph_list[[1]] <- pheatmap::pheatmap(data_to_compare[[1]],
    cluster_rows = F,
    cluster_cols = F,
    color = col_pal_sim,
    breaks = sim_breaks,
    show_rownames = show_row_labels,
    show_colnames = show_col_labels,
    silent = TRUE
  )$gtable

  ph_list[[2]] <- pheatmap::pheatmap(data_to_compare[[2]],
    cluster_rows = F,
    cluster_cols = F,
    color = col_pal_expr,
    breaks = expr_breaks,
    show_rownames = show_row_labels,
    show_colnames = show_col_labels,
    silent = TRUE
  )$gtable

  ph_list[[3]] <- pheatmap::pheatmap(data_to_compare[[3]],
    cluster_rows = F,
    cluster_cols = F,
    color = col_pal_expr,
    breaks = expr_breaks,
    show_rownames = show_row_labels,
    show_colnames = show_col_labels,
    silent = TRUE
  )$gtable

  # Combine these in a grid and save
  combine_pheatmaps(ph_list, save_name = save_name, main = main, font_size = font_size)
}

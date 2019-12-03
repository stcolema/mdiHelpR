#!/usr/bin/env Rscript

#' @title Heatmap Wrapper PSM, Data, Corr
#' @importFrom pheatmap pheatmap
#' @export
heatmapWrapperPSMDataCorr <- function(sim_mat,
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
    expr_breaks <- defineBreaks(col_pal_expr)
  }
  
  if (is.null(sim_breaks)){
    sim_breaks <- defineBreaks(col_pal_sim)
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
  compareHeatmapPSMandDataCor(sim_mat, expr_mat, corr_mat,
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

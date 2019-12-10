#!/usr/bin/env Rscript

#' @importFrom pheatmap pheatmap
#' @export
compareHeatmapPSMandDataCor <- function(...,
                                        save_name = "joint_heatmaps.png",
                                        main = "compare_heatmaps",
                                        col_pal_sim = grDevices::colorRampPalette(c("#FF9900", "white", "#146EB4"))(100),
                                        col_pal_expr = grDevices::colorRampPalette(c("#146EB4", "white", "#FF9900"))(100),
                                        expr_breaks = NULL,
                                        sim_breaks = NULL,
                                        font_size = 20,
                                        show_row_labels = FALSE,
                                        show_col_labels = FALSE) {
  if (is.null(expr_breaks)) {
    expr_breaks <- defineBreaks(col_pal_expr)
  }
  
  if (is.null(sim_breaks)){
    sim_breaks <- defineBreaks(col_pal_sim)
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
  combinePheatmaps(ph_list, save_name = save_name, main = main, font_size = font_size)
}

#!/usr/bin/env Rscript

#' @title Combine Pheatmaps
#' @importFrom cowplot plot_grid draw_label
#' @importFrom ggplot2 ggsave
#' @export
combinePheatmaps <- function(ph_list,
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
#!/usr/bin/env Rscript

#' @title Draw legend
#' @importFrom grid grid.pretty rectGrob textGrob grobTree gpar
draw_legend = function(color, breaks, ...){
  color = color[!is.infinite(breaks)]
  breaks = breaks[!is.infinite(breaks)]
  
  legend <- grid::grid.pretty(range(as.vector(breaks)))
  names(legend) <- legend
  
  height = min(unit(1, "npc"), unit(150, "bigpts"))
  
  legend_pos = (legend - min(breaks)) / (max(breaks) - min(breaks))
  legend_pos = height * legend_pos + (unit(1, "npc") - height)
  
  breaks = (breaks - min(breaks)) / (max(breaks) - min(breaks))
  breaks = height * breaks + (unit(1, "npc") - height)
  
  h = breaks[-1] - breaks[-length(breaks)]
  
  rect = grid::rectGrob(x = 0, y = breaks[-length(breaks)], width = unit(10, "bigpts"), height = h, hjust = 0, vjust = 0, gp = grid::gpar(fill = color, col = "#FFFFFF00"))
  text = grid::textGrob(names(legend), x = unit(14, "bigpts"), y = legend_pos, hjust = 0, gp = grid::gpar(...))
  
  res = grid::grobTree(rect, text)
  
  return(res)
}

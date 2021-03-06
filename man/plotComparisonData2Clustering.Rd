% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotComparisonData2Clustering.R
\name{plotComparisonData2Clustering}
\alias{plotComparisonData2Clustering}
\title{Plot comparison expression to clustering}
\usage{
plotComparisonData2Clustering(
  compare_tibble,
  dataset_names,
  num_datasets,
  file_path,
  file_type,
  gen_title = "Comparison of clustering and gene expression data",
  col_pal_sim = (grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 7, name =
    "Blues")))(100),
  col_pal_expr = (grDevices::colorRampPalette(c("#146EB4", "white", "#FF9900")))(100),
  sim_breaks = NULL,
  expr_breaks = NULL,
  show_row_labels = FALSE
)
}
\description{
Plot comparison expression to clustering
}

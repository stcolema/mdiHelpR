#!/usr/bin/env Rscript

#' @title Find order
#' @description Given a matrix, m, finds the ordering of the rows as imposed by 
#' hclust.
#' @param m A matrix.
#' @param method The linkage method used by ``hclust``.
#' @param distance The metric used by ``dist``.
#' @return A vector of integers corresponding to the row numbers of an ordered m.
#' @export
findOrder <- function(m, method = "complete", distance = "euclidean"){
  hclust(dist(m, method = distance), method = method)$order
}

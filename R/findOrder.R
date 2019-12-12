#!/usr/bin/env Rscript

#' @title Find order
#' @description Given a matrix, m, finds the ordering of the rows as imposed by 
#' hclust.
#' @param m A matrix.
#' @return A vector of integers corresponding to the row numbers of an ordered m.
#' @export
findOrder <- function(m){
  hclust(dist(m))$order
}
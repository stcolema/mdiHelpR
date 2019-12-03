#!/usr/bin/env Rscript

#' @title Arandi 4 apply
#' @importFrom mcclust arandi
#' @export
arandi4Apply <- function(my_vec, break_col, end_col){
  
  my_vec_ <- unlist(my_vec)
  
  mcclust::arandi(my_vec_[1:break_col], my_vec_[(break_col+1) : end_col])
  
}
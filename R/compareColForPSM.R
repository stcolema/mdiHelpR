#!/usr/bin/env Rscript

# Functions to create a Posterior similarity matrix

# Compare the ith column of a data.table to the [i+1,...,n]th columns
#' @title Compare col for PSM
compareColForPSM <- function(i, my_dt){
  
  # The column of interest
  # print(str(my_dt[, 1]))
  # print(str(my_dt[, i]))
  # print(str(my_dt[, ..i]))
  
  # x <- my_dt[, ..i] %>% c() %>% unlist()
  x <- my_dt[, i] %>% c() %>% unlist()
  # x1 <- my_dt[, 1] %>% c() %>% unlist()
  
  # print(str(x == x1))
  
  # The indices of the columns to drop
  drop <- 1:i
  
  # A fill to ensure the output is the same lenght for every column (possibly
  # unnecessary)
  fill <- rep(0, i)
  
  # The number of entries exactly the same between the ith column and the 
  # [i+1,...,n]th columns
  
  # print(str(my_dt[, -1]))
  # print(str(my_dt[, -drop]))
  # print(str(my_dt[, -..drop]))
  
  # y <- (x == my_dt[, -..drop]) %>% colSums()

  if(i == ncol(my_dt) - 1){
    y <- (x == my_dt[, -drop]) %>% as.data.table() %>% colSums()
  } else {
    y <- (x == my_dt[, -drop]) %>% colSums()
  } 
  
  # Fill with 0s
  c(fill, y)
}

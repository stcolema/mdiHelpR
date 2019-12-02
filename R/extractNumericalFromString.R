#!/usr/bin/env Rscript

# Given a string of the format "aaaa_111" extract the numbers (in this case "111")
#' @title Extract numerical from string
#' @export
extractNumericalFromString <- function(my_string) {
  gsub("[^0-9.]", "", my_string)
}

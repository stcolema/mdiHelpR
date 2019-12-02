#!/usr/bin/env Rscript

# From Stack Exchange https://stackoverflow.com/questions/22908050/formula-evaluation-with-mutate
# Use dplyr::mutate with a formula encoded as a string (used as we do not want
# to restrict the number of variables present)
#' @title string mutate
#' @description Given a formula captured in a string, creates a new variable 
#' defined by that formula. Created to allow for dynamic combinations when the 
#' names and number of variables present is allowed to vary.
#' @param df A data.frame.
#' @param str_formula A string enclosing a formula referring to variables in df.
#' @param varname A string that is the name of the new variable created.
#' @return A data.frame of the form of df with an additional column defined by 
#' varname and str_formula.
#' @example stringMutate(mpg, "hwy - cty", "diff")
#' @example
#' # This does not make much sense - it's purely demonstrative
#' numerical_cols <- unlist(lapply(mpg, is.numeric))
#' vars_to_add <- colnames(mpg)[numerical_cols]
#' str_formula <- paste(vars_to_add, collapse = "+")
#' new_df <- stringMutate(mpg, str_formula, "Total")
#' @example stringMutate(randu, "sqrt(x**2 + y**2 + z**2)", "dist")
#' @importFrom dplyr mutate
#' @export
stringMutate <- function(df, str_formula, varname) {
  q <- quote(dplyr::mutate(df, !!varname:= str_formula))
  eval(parse(text = sub("str_formula", str_formula, deparse(q))))
}

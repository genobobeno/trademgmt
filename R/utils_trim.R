#' Trim space from ends of string
#' 
#' Yup
#'
#' @param x string
#' 
#' @return string
#' 
utils_trim <- function (x) {gsub("^\\s+|\\s+$", "", x)}
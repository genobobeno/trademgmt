#' Change Domain to Hyperlink
#' 
#' Yup
#'
#' @param val1 href domain
#' @param val2 text
#' 
#' @return string
utils_createLink <- function(val1,val2) {
  sprintf('<a href="%s" target="_blank" class="btn btn-primary">%s</a>',val1,val2)
}

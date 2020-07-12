#' Create Numbered List
#' 
#' Yup
#'
#' @param Names vector of strings
#' 
#' @return list
utils_createNumList = function(Names,nums=NA) {
  if (is.na(nums)) {
    list(setNames(object = 1:length(Names),nm = Names))
  } else if (length(nums)==length(Names)) {
    list(setNames(object = nums,nm = Names))
  }
}

#' Create Numbered List
#' 
#' Yup
#'
#' @param Names vector of strings
#' 
#' @return list
utils_createNumList = function(Names,nums=NA) {
  if (is.na(nums)) {
    setNames(object = 1:length(Names),nm = Names)
  } else if (length(nums)==length(Names)) {
    setNames(object = nums,nm = Names)
  }
}

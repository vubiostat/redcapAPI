#' @name isZeroCodedCheckField
#' @title Identify Check Fields with a Zero Coded Option
#' 
#' @description Check fields that have 0 as a coding option can confuse
#'   certain data processing steps because it can be difficult to 
#'   differentiate if a "0" value represents an unchecked or checked 
#'   option. Identifying these fields is important to handling them
#'   correctly.
#'
#' @param field_name \code{character(1)} The name of a field to be tested.

isZeroCodedCheckField <- function(field_name){
  suffix <- sub(pattern = REGEX_CHECKBOX_FIELD_NAME,  # defined in constants.R
                replacement = "\\2", 
                x = field_name, 
                perl = TRUE)
  suffix == "0"
}

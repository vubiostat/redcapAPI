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
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = field_name, 
                              len = 1, 
                              any.missing = FALSE, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  suffix <- sub(pattern = REGEX_CHECKBOX_FIELD_NAME,  # defined in constants.R
                replacement = "\\2", 
                x = field_name, 
                perl = TRUE)
  suffix == "0"
}

warnOfZeroCodedCheckCasting <- function(field_name, x){
  if (isZeroCodedCheckField(field_name) &&
      is.factor(x) &&
      any(levels(x) %in% "0")){
    warning(sprintf("Zero-coded check field `%s` may not have been cast correctly.", 
                    field_name))
  }
}

warnZeroCodedFieldPresent <- function(field_names){
  lgl_zero_coded <- vapply(field_names, 
                               isZeroCodedCheckField, 
                               logical(1))
  if (any(lgl_zero_coded)){
    zero_coded <- field_names[lgl_zero_coded]
    warning(sprintf("Zero-coded check fields found. Verify that casting is correct for %s", 
                    paste0(zero_coded, collapse = ", ")))
  }
}

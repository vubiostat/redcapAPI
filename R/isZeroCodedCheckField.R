#' @name isZeroCodedCheckField
#' @title Identify Check Fields with a Zero Coded Option
#'
#' @description Check fields that have `0` as a coding option can confuse
#'   certain data processing steps because it can be difficult to
#'   differentiate if a `0` value represents an unchecked or checked
#'   option. Identifying these fields is important to handling them
#'   correctly.
#'
#' @param field_name `character(1)` The name of a field to be tested.
#' @param field_names `character` vector of field names.
#' @param warn_zero_coded `logical(1)`. Turn on or off warnings about zero coded fields. Defaults to `TRUE`.
#' @param x `atomic` object.
#'
#' @section Zero-Coded Check Fields:
#' A zero-coded check field is a field of the REDCap type `checkbox` that has
#' a coding definition of `0, [label]`. When exported, the field names for
#' these fields is `[field_name]___0`. As in other checkbox fields, the
#' raw data output returns binary values where 0 represent an unchecked
#' box and 1 represents a checked box. For zero-coded checkboxes, then, a
#' value of 1 indicates that 0 was selected.
#'
#' This coding rarely presents a problem when casting from raw values
#' (as is done in `exportRecordsTyped`). However, casting from coded or
#' labeled values can be problematic. In this case, it becomes
#' indeterminate from context if the intent of `0` is 'false' or the coded
#' value '0' ('true') ...
#'
#' The situations in which casting may fail to produce the desired results are
#'
#' | Code | Label | Result |
#' |------|-------|--------|
#' | 0    | anything other than "0" | Likely to fail when casting from coded values |
#' | 0    | 0     | Likely to fail when casting from coded or labeled values |
#'
#' Because of the potential for miscast data, casting functions will issue
#' a warning anytime a zero-coded check field is encountered. A separate
#' warning is issued when a field is cast from coded or labeled values.
#'
#' When casting from coded or labeled values, it is strongly recommended that
#' the function [castCheckForImport()] be used. This function permits the
#' user to state explicitly which values should be recognized as checked,
#' avoiding the ambiguity resulting from the coding.
#'
#' @return
#' `isZeroCodedCheckField` returns a `logical(1)`
#'
#' `warnOfZeroCodedCheckCasting` has no return and issues a warning if the
#'   field name appears to be zero-coded.
#'
#' `warnZeroCodedFieldPresent` has no return and issues a warning if any
#'   of the fields passed appear to be zero-coded.
#'
#' @examples
#' \dontrun{
#' isZeroCodedCheckField("check_field___x")
#'
#' isZeroCodedCheckField("check_field___0")
#'
#'
#' x <- factor(c(1, 0, 1, 0, 0),
#'             levels = 0:1)
#' warnOfZeroCodedCheckCasting(field_name = "check_field___0",
#'                             x = x)
#'
#'
#' warnZeroCodedFieldPresent(c("check_field___x", "check_field___0"), TRUE)
#' }

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

#' @rdname isZeroCodedCheckField

warnOfZeroCodedCheckCasting <- function(field_name, x){
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_character(x = field_name,
                              len = 1,
                              add = coll)

  checkmate::assert_atomic(x = x,
                           add = coll)

  checkmate::reportAssertions(coll)

  if (isZeroCodedCheckField(field_name) &&
      is.factor(x) &&
      any(levels(x) %in% "0")){
    logWarning(sprintf("Zero-coded check field `%s` may not have been cast correctly. See `?exportRecordsTyped`, Zero-Coded Check Fields.",
                    field_name))
  }
}

#' @rdname isZeroCodedCheckField

warnZeroCodedFieldPresent <- function(field_names, warn_zero_coded)
{
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_character(x = field_names,
                              any.missing = FALSE,
                              add = coll)

  checkmate::assert_logical(x = warn_zero_coded,
                            len = 1,
                            any.missing = FALSE,
                            add = coll)

  checkmate::reportAssertions(coll)

  if(!warn_zero_coded) return(NULL)

  lgl_zero_coded <- vapply(field_names,
                               isZeroCodedCheckField,
                               logical(1))
  if (any(lgl_zero_coded)){
    zero_coded <- field_names[lgl_zero_coded]
    logWarning(sprintf("Zero-coded check fields found. Verify that casting is correct for %s. See `?exportRecordsTyped`, Zero-Coded Check Fields.",
                    paste0(zero_coded, collapse = ", ")))
  }
}

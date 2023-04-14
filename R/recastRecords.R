#' @name recastRecords
#' @title Recast Data Fields
#' 
#' @description Allows for recasting one or more fields using 
#'   the codes and labels in the Meta Data.
#'   
#' @param data \code{data.frame} with the data fields to be recoded. 
#' @param rcon \code{recapConnection} object.
#' @param fields \code{character/logical/integerish}. A vector for identifying
#'   which fields to recode. When \code{logical}, the length must match 
#'   the number of columns in \code{data} (i.e., recycling not permitted).
#'   A message is printed if any of the indicated fields are not a 
#'   multiple choice field; no action will be taken on such fields.
#'   For this function, yes/no and true/false fields are considered 
#'   multiple choice fields. Fields of class \code{mChoice} are quietly skipped.
#' @param cast A named \code{list} of user specified class casting functions. 
#'   Keys must correspond to a truncated REDCap field type, i.e.
#'   {date_, datetime_, datetime_seconds_, time_mm_ss, time_hh_mm_ss, time, float,
#'   number, calc, int, integer, select, radio, dropdown, yesno, truefalse,
#'   checkbox, form_complete, sql}. The function will be 
#'   provided the variables (x, field_name, coding). 
#'   See \code{\link{fieldValidationAndCasting}}
#' @param suffix \code{character(1)}. An optional suffix to provide if 
#'   the recoded variables should be returned as new columns. For example, 
#'   if recoding a field \code{forklift_brand} and \code{suffix = "_labelled"}, 
#'   the result will have one column with the coded values 
#'   (\code{forklift_brand}) and one column with the labelled values 
#'   (\code{forklift_brand_labelled}).
#'   
#' @details field types for which no casting function is specified will
#'   be returned with no changes.
#'   
#'   The default \code{cast} list in 
#'   \code{recastRecords} inverts the default casting of \code{exportRecordsTyped}. 
#'   
#' @export

recastRecords <- function(data, 
                       rcon, 
                       fields, 
                       cast = invert_default_cast, 
                       suffix    = ""){
  ###################################################################
  # Argument Validation #############################################
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_data_frame(x = data, 
                               add = coll)
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapConnection", 
                          add = coll)
  
  checkmate::assert(
    checkmate::test_character(x = fields), 
    checkmate::test_logical(x = fields), 
    checkmate::test_integerish(x = fields, lower = 0), 
    add = coll
  )
  
  checkmate::assert_list(x = cast, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_character(x = suffix, 
                              len = 1, 
                              any.missing = FALSE,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (is.numeric(fields)){
    out_of_range <- fields[fields > ncol(data)]
    if (length(out_of_range) > 0){
      coll$push(sprintf("Columns {%s} requested in a data frame with %s columns", 
                        paste0(out_of_range, collapse = ", "), 
                        ncol(data)))
    }
  } 
  
  if (is.logical(fields)){
    if (length(fields) != ncol(data)){
      coll$push(sprintf("'fields' (logical) should be of length %s and is length %s", 
                        ncol(data), 
                        length(fields)))
    }
  } 
  
  checkmate::reportAssertions(coll)
  
  if (!is.character(fields)) fields <- names(data)[fields]
  
  checkmate::assert_subset(x = fields, 
                           choices = names(data), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Remove mChoice fields from processing                        ####
  
  is_mChoice <- vapply(data[fields], 
                       FUN = inherits, 
                       what = "mChoice", 
                       FUN.VALUE = logical(1))
  fields <- fields[!is_mChoice]
  
  ###################################################################
  # Derive field information
  MetaData <- rcon$metadata()
  
  field_names <- fields
  field_bases <- gsub("___.+$", "", field_names)
  field_text_types <- MetaData$text_validation_type_or_show_slider_number[match(field_bases, MetaData$field_name)]
  field_map <- match(field_bases, MetaData$field_name)
  
  field_types <- .exportRecordsTyped_getFieldTypes(rcon = rcon, 
                                                   field_map = field_map,
                                                   field_bases = field_bases, 
                                                   field_text_types = field_text_types)
  
  ###################################################################
  # Derive codings (This is probably a good internal helper)
  
  codings <- .exportRecordsTyped_getCodings(rcon = rcon, 
                                            field_map = field_map, 
                                            field_names = field_names, 
                                            field_types = field_types, 
                                            code_check = TRUE)

  data <- .exportRecordsTyped_recastRecords(Raw = data, 
                                            cast = cast, 
                                            field_types = field_types, 
                                            codings = codings, 
                                            field_names = field_names, 
                                            suffix = suffix)
  
  data
}

#' #' @rdname recastRecords
#' #' @export
#' 
#' invert_default_cast <- list(checkbox = uncastChecked, 
#'                             dropdown = uncastLabel,
#'                             radio = uncastLabel, 
#'                             yesno = function(x, field_name, coding) as.numeric(x == "Yes"),
#'                             truefalse = as.numeric)

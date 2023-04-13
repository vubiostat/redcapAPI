#' @name recastData
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
#'   multiple choice fields.
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
#' @export

recastData <- function(data, 
                       rcon, 
                       fields, 
                       cast = list(checkbox = uncastChecked, 
                                   dropdown = uncastLabel,
                                   radio = uncastLabel, 
                                   yesno = uncastLabel,
                                   truefalse = as.numeric), 
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
    if (out_of_range > 0){
      coll$push(sprintf("Columns {%s} requested in a data frame with %s columns", 
                        paste0(out_of_range, collapse = ", "), 
                        ncol(data)))
    }
  } 
  
  if (is.logical(fields)){
    if (length(fields) > ncol(data)){
      coll$push(sprintf("fields (logical) should be of length %s and is length %s", 
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
  
  for(i in seq_along(field_names))
  {
    if(field_types[i] %in% names(cast))
    {
      this_field_name <- sprintf("%s%s", 
                                 fields[i], 
                                 suffix)
      x <- data[[ field_names[i] ]]
      this_attribute <- attributes(x)
      this_attribute <- this_attribute[!names(this_attribute) %in% c("class", "levels")]
      
      typecast <- cast[[ field_types[i] ]]
      if(is.function(typecast)){
        data[[ this_field_name ]] <- typecast(as.character(x), field_name=field_names[i], coding=codings[[i]])
        attributes(data[[ this_field_name ]]) <- this_attribute
      }
    }
  }
  
  data
}

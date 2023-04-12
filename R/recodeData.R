#' @name recodeData
#' @title Recode Multiple Choice Fields
#' 
#' @description Allows for recoding one or more multiple choice fields using 
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
#' @param return_as \code{character}, one of \code{c("coded", "labelled")}. 
#'   Determines which set of values are returned. 
#' @param suffix \code{character(1)}. An optional suffix to provide if 
#'   the recoded variables should be returned as new columns. For example, 
#'   if recoding a field \code{forklift_brand} and \code{suffix = "_labelled"}, 
#'   the result will have one column with the coded values 
#'   (\code{forklift_brand}) and one column with the labelled values 
#'   (\code{forklift_brand_labelled}).
#'   
#' @export

recodeData <- function(data, 
                       rcon, 
                       fields, 
                       return_as = c("coded", "labelled"), 
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
  
  return_as <- checkmate::matchArg(x = return_as, 
                                   choices = c("coded", "labelled"), 
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
  
  checkmate::assert_subset(x = fields, 
                           choices = rcon$metadata()$field_name, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Remove fields without a codebook ################################
  
  MetaData <- rcon$metadata()
  
  MetaDataChoice <- MetaData[MetaData$field_type %in% c("checkbox", "dropdown", "radio", "truefalse", "yesno"), ]
  
  fields_to_remove <- fields[!fields %in% MetaDataChoice$field_name]
  
  if (length(fields_to_remove) > 0){
    message("The following fields are not multiple choice fields and will be ignored: ", 
            paste0(fields_to_remove, collapse = ", "))
    fields <- fields[!fields %in% fields_to_remove]
  }
  
  ###################################################################
  # Start Recoding ##################################################
  
  MetaDataChoice <- MetaDataChoice[MetaDataChoice$field_name %in% fields, ]
  
  field_to <- sprintf("%s%s", fields, suffix)
  
  if (return_as == "coded"){
    from <- "choice_label"
    to <- "choice_value"
  } else {
    from <- "choice_value"
    to <- "choice_label"
  }
  
  for (i in seq_along(fields)){
    this_attribute <- attributes(data[[ fields[i] ]])

    this_codebook <- .recodeData_getCodebook(fields[i], MetaDataChoice)
    
    data[[field_to[i] ]] <- 
      this_codebook[, to][match(data[[ fields[i] ]], this_codebook[, from])]
    attributes(data[[ field_to[i] ]]) <- this_attribute
  }
  
  data
}

#####################################################################
# Unexported ########################################################

.recodeData_getCodebook <- function(field_name, MetaDataChoice){
  field_type <- MetaDataChoice$field_type[MetaDataChoice$field_name == field_name]

  if (field_type == "truefalse"){
    matrix(c("0", "FALSE", "1", "TRUE"), 
           ncol = 2, 
           byrow = TRUE, 
           dimnames = list(NULL, c("choice_value", "choice_label")))
  } else if (field_type == "yesno"){
    matrix(c("0", "No", "1", "Yes"), 
           ncol = 2, 
           byrow = TRUE, 
           dimnames = list(NULL, c("choice_value", "choice_label")))
  } else {
    fieldChoiceMapping( MetaDataChoice$select_choices_or_calculations[MetaDataChoice$field_name == field_name] )
  }
}

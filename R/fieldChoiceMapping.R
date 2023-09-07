#' @name fieldChoiceMapping
#' @title Split a Field Choice Mapping Into a Two Column Matrix
#' 
#' @description Uses the string from the `select_choices_or_calculations`
#'   for the meta data to create a matrix of codes and their mapped 
#'   labels. 
#'   
#' @param object `redcapConnection` or `character(1)`. When `character`, is 
#'   matches the format of the meta data field choices
#'   (i.e. `rcon$meta_data()$select_choices_or_calculations`).
#' @param field_name `character(1)` gives the field name for which to 
#'   make the choice mapping.
#' @inheritParams common-dot-args
#'   
#' @return 
#' Returns a matrix with two columns, `choice_value` and `choice_label`
#' 
#' @export

fieldChoiceMapping <- function(object,  field_name, ...){
  UseMethod("fieldChoiceMapping")
}

#' @rdname fieldChoiceMapping
#' @export

fieldChoiceMapping.character <- function(object, 
                                         field_name,
                                         ...){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = object, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (!(grepl(REGEX_MULT_CHOICE, # defined in constants.R 
              object, perl = TRUE))){
    coll$push(
      sprintf("'%s' choice string does not appear to be formatted for choices.", 
              field_name))
    checkmate::reportAssertions(coll)
  }
  
  mapping <- unlist(strsplit(object, "[|]"))
  #split on only the first comma. This allows commas to remain in the field label.
  # if no commas the code is likely from a legacy project
  if (!any(grepl(",", mapping))) {
    matrix <- matrix(nrow = length(mapping), ncol = 2,
                            dimnames = list(NULL, c("choice_value", "choice_label")))

    matrix[, "choice_value"] <- trimws(mapping)
    matrix[, "choice_label"] <- trimws(mapping)

    mapping <- matrix
    return(mapping)

  } else {
    mapping <- regmatches(mapping, 
                          regexec("([^,]*),(.*)", 
                                  mapping, 
                                  perl=TRUE))
    mapping <- do.call("rbind", mapping)
    mapping <- trimws(mapping[, -1, drop=FALSE]) # the first column is the original string. 

    colnames(mapping) <- c("choice_value", "choice_label")
    return(mapping)
  }
}

#' @rdname fieldChoiceMapping
#' @export

fieldChoiceMapping.redcapApiConnection <- function(object, 
                                                  field_name, 
                                                  ...){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = object, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_character(x = field_name, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  MetaData <- object$metadata()
  
  if (!field_name %in% MetaData$field_name){
    coll$push(sprintf("'%s' is not a field listed in the meta data", 
                      field_name))
    checkmate::reportAssertions(coll)
  }
  
  MetaData <- MetaData[MetaData$field_name == field_name, ]
  
  if (!MetaData$field_type %in% c("checkbox", "dropdown", "radio")){
    coll$push(sprintf("'%s' is not a checkbox, dropdown, or radio field", 
                      field_name))
    checkmate::reportAssertions(coll)
  }
  
  field_choice <- MetaData$select_choices_or_calculations[MetaData$field_name == field_name]
  
  fieldChoiceMapping(field_choice)
}

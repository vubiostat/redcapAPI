#' @name fieldChoiceMapping
#' @title Splits a Field Choice Mapping Into a Two Column Matrix
#' 
#' @description Uses the string from the \code{select_choices_or_calculations}
#'   for the meta data to create a matrix of codes and their mapped 
#'   labels. 
#'   
#' @param object May be either a \code{character(1)} with the content of the 
#'   field choices (i.e. \code{meta_data$select_choices_or_calculations}), 
#'   or a \code{redcapConnection} object.
#' @param field_name \code{character(1)} gives the field name for which to 
#'   make the choice mapping.
#' @param ... arguments to pass to other methods.
#'   
#' @return 
#' Returns a matrix with two columns, \code{choice_value} and \code{choice_label}
#' 
#' @author Benjamin Nutter, Shawn Garbett
#' 
#' @source 
#' https://stackoverflow.com/questions/23961022/split-strings-on-first-and-last-commas
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
  
  if (!(grepl("[^\\|]+,[^\\|]*(?:\\|[^\\|]+,[^\\|]*)*", object))){
    coll$push(
      sprintf("'%s' choice string does not appear to be formatted for choices.", 
              field_name))
    checkmate::reportAssertions(coll)
  }
  
  mapping <- unlist(strsplit(object, "[|]"))
  # split on only the first comma. This allows commas to remain in the field label.
  mapping <- regmatches(mapping, 
                        regexec("([^,]*),(.*)", 
                                mapping, 
                                perl=TRUE))
  mapping <- do.call("rbind", mapping)
  mapping <- trimws(mapping[, -1, drop=FALSE]) # the first column is the original string. 

  colnames(mapping) <- c("choice_value", "choice_label")
  mapping
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
  
  if (!MetaData$field_type %in% c("checkbox", "dropown", "radio")){
    coll$push(sprintf("'%s' is not a checkbox, dropdown, or radio field", 
                      field_name))
    checkmate::reportAssertions(coll)
  }
  
  field_choice <- MetaData$select_choices_or_calculations[MetaData$field_name == field_name]
  
  fieldChoiceMapping(field_choice)
}

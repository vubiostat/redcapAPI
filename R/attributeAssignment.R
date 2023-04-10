#' @rdname attributeAssignment
#' @title Helper functions for \code{exportRecordsTyped} attributes
#' @description This set of functions helps in setting attributes for
#' columns of the resulting type cast data.frame.
#'   
#' @param field_name \code{character(n)}. Name of the fields.
#' @param field_label \code{character(n)}. Labels from meta data.
#' @param field_annotation \code{character(n)}. Annotations from meta_data.
#' 
#' @details Functions passed into the \code{assignment} parameter list of 
#' \code{\link{exportRecordsTyped}} construct attributes on a column. 
#' They are expected to have a signature of \code{function(field_name,
#' field_label, field_annotation)} and return the attribute to assign or NA. 
#' They must be vectorized.
#' 
#' Useful utilities are provided in \code{\link{stringCleanup}}
#' 
#' \code{stripHTMLandUnicode} strips both HTML and UNICODE from the field_label.
#' 
#' \code{unitsFieldAnnotation} pulls a units string from the field_annotation. 
#' An example of the form searched for is \code{units=\{"meters"\}}
#' 
#' @export

stripHTMLandUnicode <- function(field_name, field_label, field_annotation)
{
  stripUnicode(stripHTMLTags(field_label))
}

#' @rdname attributeAssignment
#' @export

unitsFieldAnnotation <- function(field_name, field_label, field_annotation)
{
  m <- gsub('^.*units\\s*=\\s*\\{\\s*"([^"]*)"\\s*\\}.*$', "\\1", field_annotation)
  
  unname(ifelse(is.na(m) | sapply(m, length) == 0 | m == field_annotation,NA,m))
}
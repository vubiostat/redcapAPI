#' @rdname attributeAssignment
#' @title Helper Functions for `exportRecordsType` Attributes
#' 
#' @description These functions assist in setting attributes for
#' columns of the resulting type cast data.frame.
#'   
#' @param field_name `character`. Name of the fields.
#' @param field_label `character`. Labels from meta data.
#' @param field_annotation `character`. Annotations from meta_data.
#' 
#' @details Functions passed into the `assignment` argument list of 
#' [exportRecordsTyped()] construct attributes on a column. 
#' They are expected to have a signature of `function(field_name,
#' field_label, field_annotation)` and return the attribute to assign or `NA`. 
#' They must be vectorized.
#' 
#' Useful utilities are provided in [stringCleanup()]
#' 
#' `stripHTMLandUnicode` strips both HTML and UNICODE from the `field_label`.
#' 
#' `unitsFieldAnnotation` pulls a units string from the field_annotation. 
#' An example of the form searched for is `units=\{"meters"\}`
#' 
#' @return 
#' `stripHTMLandUnicode` returns a character vector.
#' 
#' `unitsFieldAnnotation` returns a character vector.
#' 
#' @seealso 
#' [exportRecordsTyped()], \cr
#' [exportReportsTyped()], \cr
#' [stripHTMLTags()], \cr
#' [stripUnicode()]
#' 
#' @examples
#' \dontrun{
#' stripHTMLandUnicode("field_name", "<b>Field label</b>", "field annotation")
#' 
#' unitsFieldAnnotation("field", "label", "units={\"meters\"}")
#' }
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

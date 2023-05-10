#' @name getProjectIdFields 
#' @title Return a vector of the Project ID Fields
#' 
#' @description Returns a character vector listing the project ID fields. 
#'   This will be at most a vector of length two. The first element will
#'   be the first field in the meta data. The second, if provided, will
#'   be the name of the secondary unique field specified in the project.
#'
#' @param rcon A \code{redcapConnection} object. 
#' 

getProjectIdFields <- function(rcon){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapConnection", 
                          add = coll)
  
  checkmate::reportAssertions(coll)
  
  primary_unique_field <- rcon$metadata()$field_name[1]
  secondary_unique_field <- rcon$projectInformation()$secondary_unique_field
  
  has_secondary_field <- 
    length(secondary_unique_field) > 0 && 
    isTRUE(!is.na(secondary_unique_field)) &&
    isTRUE(!trimws(secondary_unique_field) %in% "")
  
  if (has_secondary_field){
    c(primary_unique_field, secondary_unique_field)
  } else {
    primary_unique_field
  }
}
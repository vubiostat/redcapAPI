#' @name vectorToApiBodyList
#' @title Convert R Vector To List for the API Call Body
#' 
#' @description Converts an R vector to a list that will be suitable for 
#'   `makeApiCall`.
#'   
#' @param vector An `atomic` vector.
#' @param parameter_name `character(1)`. The REDCap API parameter name.
#' 
#' @examples
#' \dontrun{
#' vectorToApiBodyList(1:3, "records")
#' }
#' 
#' @export

vectorToApiBodyList <- function(vector, 
                                parameter_name){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_atomic(x = vector, 
                           add = coll)
  
  checkmate::assert_character(x = parameter_name, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functional Code -------------------------------------------------
  rv <- lapply(vector, identity)
  names(rv) <- sprintf("%s[%s]", 
                       parameter_name, 
                       seq_along(rv))
  rv
}

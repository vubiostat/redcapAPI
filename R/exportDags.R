# Complete documentation in documentation.R
#' @describeIn dagMethods Export Data Access Groups from a REDCap Project
#' @order 1
#' @export

exportDags <- function(rcon, ...){
  UseMethod("exportDags")
}

#' @rdname dagMethods
#' @order 4
#' @export

exportDags.redcapApiConnection <- function(rcon, 
                                           ...)
{
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)

  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make the API Body list                                       ####
  
  body <- list(content = "dag", 
               format = "csv", 
               returnFormat = "csv")

  ###################################################################
  # Call the API                                                 ####
  as.data.frame(makeApiCall(rcon, body, ...))
}

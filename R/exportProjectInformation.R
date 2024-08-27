#' @describeIn projectInformationMethods Export project settings.
#' @order 1
#' @export

exportProjectInformation <- function(rcon, 
                                     ...){
  UseMethod("exportProjectInformation")
}

#' @rdname projectInformationMethods
#' @order 3
#' @export

exportProjectInformation.redcapApiConnection <- function(rcon, 
                                                         ...)
{
   ##################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)

  checkmate::reportAssertions(coll)
  
   ##################################################################
  # Make the Body List
  
  body <- list(content = 'project',
               format = 'csv',
               returnFormat = 'csv')

   ##################################################################
  # Call the API
  as.data.frame(makeApiCall(rcon, body, ...))
}

#' @describeIn recordsManagementMethods Get the ID number for the next record to be created.
#' @order 1
#' @export

exportNextRecordName <- function(rcon, 
                                 ...){
  UseMethod("exportNextRecordName")
}

#' @rdname recordsManagementMethods
#' @order 3
#' @export


exportNextRecordName.redcapApiConnection <- function(rcon, 
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
  body <- list(content = 'generateNextRecordName')

   ##################################################################
  # Call the API
  as.numeric(as.character(makeApiCall(rcon, body, ...)))
}

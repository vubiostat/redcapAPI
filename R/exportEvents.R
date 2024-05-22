#' @describeIn eventsMethods Export events from a REDCap project.
#' @order 1
#' @export

exportEvents <- function(rcon, 
                         ...){
  UseMethod("exportEvents")
}

#' @rdname eventsMethods
#' @order 4
#' @export

exportEvents.redcapApiConnection <- function(rcon, 
                                             arms           = NULL, 
                                             ...)
{
  if (is.character(arms)) arms <- as.numeric(arms)
  
   ##################################################################
  # Argument Validation 
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  checkmate::assert_integerish(x = arms,
                               null.ok = TRUE,
                               add = coll)

  checkmate::reportAssertions(coll)
  
  Arms <- rcon$arms()
  
  checkmate::assert_subset(x = arms, 
                           choices = Arms$arm_num, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
   ##################################################################
  # Return for Classical projects
  
  if (rcon$projectInformation()$is_longitudinal == 0){
    return(REDCAP_EVENT_STRUCTURE) # Defined in redcapDataStructure.R
  }
  
   ##################################################################
  # Make the Body List
  
  body <- list(content = 'event', 
               format = 'csv', 
               returnFormat = 'csv')
  body <- c(body, 
            vectorToApiBodyList(arms, "arms"))

   ##################################################################
  # Call the API

  response <- as.data.frame(makeApiCall(rcon, body, ...))
  if(nrow(response) == 0) REDCAP_EVENT_STRUCTURE else response
}

#' @describeIn mappingMethods Export instrument-event mappings.  
#' @order 1
#' @export 

exportMappings <- function(rcon, 
                           arms, 
                           ...){
  UseMethod("exportMappings")
}

#' @rdname mappingMethods
#' @order 3
#' @export

exportMappings.redcapApiConnection <- function(rcon, 
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
  
  checkmate::assert_character(x = arms,
                              null.ok = TRUE,
                              any.missing = FALSE,
                              add = coll)

  checkmate::reportAssertions(coll)
  
   ##################################################################
  # Return empty data frame for classical projects
  
  if (rcon$projectInformation()$is_longitudinal == 0){
    return(data.frame(arm_num = numeric(0), 
                      unique_event_name = character(0), 
                      form = character(0)))
  }
  
   ##################################################################
  # Make Body List
  body <- list(content = 'formEventMapping', 
               format = 'csv')
  
  body <- c(body, 
            vectorToApiBodyList(arms, "arms"))

   ##################################################################
  # Call the API
  as.data.frame(makeApiCall(rcon, body, ...))
}

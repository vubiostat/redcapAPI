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
                                             ...,
                                             error_handling = getOption("redcap_error_handling"), 
                                             config         = list(), 
                                             api_param      = list()){
  
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
  
  error_handling <- checkmate::matchArg(x = error_handling, 
                                        choices = c("null", "error"),
                                        .var.name = "error_handling",
                                        add = coll)
  
  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         names = "named", 
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
  
  body <- list(token = rcon$token, 
               content = 'event', 
               format = 'csv', 
               returnFormat = 'csv')
  body <- c(body, 
            vectorToApiBodyList(arms, "arms"))
  
  body <- body[lengths(body) > 0]
  
   ##################################################################
  # Call the API
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) return(redcapError(response, error_handling))

  response <- as.data.frame(response)
  if(nrow(response) == 0) REDCAP_EVENT_STRUCTURE else response
}

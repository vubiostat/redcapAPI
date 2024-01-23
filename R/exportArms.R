#' @describeIn armsMethods Export the arms defined in a project.
#' @order 1
#' @export

exportArms <- function(rcon, ...){
  UseMethod("exportArms")
}

#' @rdname armsMethods
#' @order 4
#' @export

exportArms.redcapApiConnection <- function(rcon, 
                                           arms = character(0), 
                                           ...,
                                           error_handling = getOption("redcap_error_handling"), 
                                           config = list(), 
                                           api_param = list())
{
  if (is.numeric(arms)) arms <- as.character(arms)
  
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  checkmate::assert_character(x = arms,
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
  
  if(rcon$projectInformation()$is_longitudinal == 0){
    return(REDCAP_ARMS_STRUCTURE) # defined in redcapDataStructure.R
  }
  
  # Build the body list ---------------------------------------------
  body <- c(list(token = rcon$token, 
                 content = 'arm', 
                 format = 'csv', 
                 returnFormat = 'csv'), 
            vectorToApiBodyList(arms, 
                                parameter_name = "arms"))
  
  body <- body[lengths(body) > 0]
  
  
  # API Call --------------------------------------------------------
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200){
    redcapError(response, 
                 error_handling = error_handling)
  }
  
  as.data.frame(response)
}

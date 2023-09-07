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
  
  checkmate::assert_character(x = arms,
                              null.ok = TRUE,
                              any.missing = FALSE,
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
  
   ##################################################################
  # Return empty data frame for classical projects
  
  if (rcon$projectInformation()$is_longitudinal == 0){
    return(data.frame(arm_num = numeric(0), 
                      unique_event_name = character(0), 
                      form = character(0)))
  }
  
   ##################################################################
  # Make Body List
  body <- list(token = rcon$token, 
               content = 'formEventMapping', 
               format = 'csv')
  
  body <- c(body, 
            vectorToApiBodyList(arms, "arms"))

  body <- body[lengths(body) > 0]
  
   ##################################################################
  # Call the API
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) return(redcapError(response, error_handling))
  
  utils::read.csv(text = as.character(response), 
                  stringsAsFactors = FALSE, 
                  na.strings = "")
}

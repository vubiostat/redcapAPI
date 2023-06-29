#' @name exportDags
#' @title Export Data Access Groups
#' 
#' @description Export the Data Access Groups (DAGs) associated with a project.
#'   Returns a listing of the data access group, the unique name, and the
#'   integer ID number.
#'   
#' @param rcon \code{redcapConnection} object
#' @param ... Additional arguments to pass to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'   
#' @export

exportDags <- function(rcon, ...){
  UseMethod("exportDags")
}

#' @rdname exportDags
#' @export

exportDags.redcapApiConnection <- function(rcon, 
                                           ...,
                                           error_handling = getOption("redcap_error_handling"), 
                                           config         = list(), 
                                           api_param      = list()){
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
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
  
  ###################################################################
  # Make the API Body list                                       ####
  
  body <- list(content = "dag", 
               format = "csv", 
               returnFormat = "csv")
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API                                                 ####
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) return(redcap_error(response, error_handling))
  
  read.csv(text = as.character(response), 
           na.strings = "",
           stringsAsFactors = FALSE)
}

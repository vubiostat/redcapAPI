#' @name importMappings
#' @title Import Instrument Event Mapping
#' 
#' @description This method allows you to import Instrument-Event Mappings 
#'   into a project (this corresponds to the 'Designate Instruments for 
#'   My Events' page in the project).
#' 
#' NOTE: This only works for longitudinal projects.
#'   
#' @param rcon a \code{redcapConnection} object. 
#' @param data \code{data.frame} with columns \code{arm_num}, 
#'   \code{unique_event_name}, and \code{form}. See Details
#' @param refresh \code{logical(1)}. When \code{TRUE}, cached mappings 
#'   in the \code{rcon} object are refreshed after the import.
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
#' @details The \code{arm_num} variable will be compared against existing arms
#'   in the project (via \code{rcon$arms()}. An error is returned if any of 
#'   the arm numbers are not found in the project.
#'   
#' The \code{unique_event_name} must exist in \code{rcon$events()}. An error
#' is returned if an value in the data doesn't match. 
#' 
#' The values of \code{form} must all match a form name in 
#' \code{rcon$instruments()}. An error is returned if any values provided 
#' do not match.
#' 
#' @export

importMappings <- function(rcon, 
                           data, 
                           refresh = TRUE, 
                           ...){
  UseMethod("importMappings")
}

#' @rdname importMappings
#' @export

importMappings.redcapApiConnection <- function(rcon, 
                                               data, 
                                               refresh = TRUE, 
                                               ..., 
                                               error_handling  = getOption("redcap_error_handling"),
                                               config          = list(), 
                                               api_param       = list()){
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = data, 
                               col.names = "named", 
                               add = coll)
  
  checkmate::assert_logical(x = refresh, 
                            len = 1, 
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
  
  checkmate::assert_subset(x = names(data), 
                           choices = names(rcon$mapping()), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  checkmate::assert_subset(x = data$arm_num, 
                           choices = rcon$arms()$arm_num, 
                           add = coll)
  
  checkmate::assert_subset(x = data$unique_event_name, 
                           choices = rcon$events()$unique_event_name, 
                           add = coll)
  
  checkmate::assert_subset(x = data$form, 
                           choices = rcon$instruments()$instrument_name, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make the API Body List                                       ####
  
  body <- list(content = "formEventMapping", 
               format = "csv", 
               returnFormat = "csv", 
               data = writeDataForImport(data))
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API                                                 ####
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != "200"){
    redcap_error(response, error_handling)
  } 
  
  message("Mappings imported: ", as.character(response))
  
  if (refresh && rcon$has_mapping()){
    rcon$refresh_mapping()
  }
}

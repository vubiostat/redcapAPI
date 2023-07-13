#' @name importDags
#' @title Import Data Access Groups to a REDCap Project
#' 
#' @description This method allows you to import new DAGs (Data Access Groups) 
#'   into a project or update the group name of any existing DAGs.
#'   
#'   NOTE: DAGs can be renamed by simply changing the group name 
#'   (data_access_group_name). DAG can be created by providing group name 
#'   value while unique group name should be set to blank.
#'   
#' @param rcon A \code{redcapConnection} object
#' @param data A \code{data.frame} with two columns: \code{data_access_group_name}
#'   and \code{unique_group_name}. 
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
#' @details To add a DAG, provide a value for \code{data_access_group_name} 
#'   with value for \code{unique_group_name}. 
#'   
#'   To modify a group name, provide a new value for \code{data_access_group_name}
#'   with the associated \code{unique_group_name}. If \code{unique_group_name}
#'   is provided, it must match a value currently in the project.
#'   
#' @export

importDags <- function(rcon, 
                       data, 
                       ...){
  UseMethod("importDags")
}

#' @rdname importDags
#' @export

importDags.redcapApiConnection <- function(rcon, 
                                           data, 
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
  
  checkmate::assert_data_frame(x = data, 
                               names = "named", 
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
  
  
  
  checkmate::assert_subset(names(data), 
                           choices = c("data_access_group_name", "unique_group_name"), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  
  
  checkmate::assert_subset(data$unique_group_name, 
                           choices = c(rcon$dags()$unique_group_name, NA_character_), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make the API Body list                                       ####
  
  body <- list(content = "dag",
               action = "import", 
               format = "csv", 
               returnFormat = "csv", 
               data = writeDataForImport(data))
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API                                                 ####
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) return(redcap_error(response, error_handling))
  
  message(sprintf("DAGs imported: %s", 
                  as.character(response)))
}

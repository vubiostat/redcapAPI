#' @name importUserDagAssignments
#' @title Import User DAG Assignments to a Project
#' 
#' @description This method allows you to assign users to any data access group.
#' 
#' NOTE: If you wish to modify an existing mapping, you must provide its 
#' unique username and group name. If the 'redcap_data_access_group' column 
#' is not provided, user will not assigned to any group. There should be only
#' one record per username.
#' 
#' @param rcon A \code{redcapConnection} object. 
#' @param data \code{data.frame} with the columns \code{username} and 
#'   \code{redcap_data_access_group}.
#' @param ... Arguments to be passed to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcapError}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'   
#' @export

importUserDagAssignments <- function(rcon, 
                                     data, 
                                     ...){
  UseMethod("importUserDagAssignments")
}

#' @rdname importUserDagAssignments
#' @export

importUserDagAssignments.redcapApiConnection <- function(rcon,
                                                         data, 
                                                         ..., 
                                                         error_handling = getOption("redcap_error_handling"), 
                                                         config = list(), 
                                                         api_param = list()){
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = data, 
                               col.names = "named", 
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
                           choices = c("username", 
                                       "redcap_data_access_group"), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  
  
  checkmate::assert_subset(x = data$username, 
                           choices = rcon$users()$username, 
                           add = coll)
  
  checkmate::assert_subset(x = data$redcap_data_access_group, 
                           choices = c(rcon$dags()$unique_group_name, NA_character_), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  
  # Each username may appear only exactly once.
  
  username_freq <- table(data$username)
  if (any(username_freq > 1)){
    username_to_report <- names(username_freq)[username_freq > 1]
    username_to_report <- paste0(username_to_report, collapse = ", ")
    
    coll$push(sprintf("Usernames should only be listed exactly once: %s", 
                      username_to_report))
  }
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Build the body list                                          ####
  
  body <- list(content = "userDagMapping", 
               action = "import", 
               format = "csv", 
               returnFormat = "csv",
               data = writeDataForImport(data))
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Make the API Call                                            ####
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200){
    redcapError(response, 
                 error_handling = error_handling)
  }
  
  message(sprintf("User-DAG Assignments Added/Modified: %s", 
                  as.character(response)))
}

# Complete documentation in documentation.R
#' @describeIn dagAssignmentMethods Import new or modified User-DAG Assignments.
#' @order 2
#' @export

importUserDagAssignments <- function(rcon, 
                                     data, 
                                     ...){
  UseMethod("importUserDagAssignments")
}

#' @rdname dagAssignmentMethods
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
  
  invisible(as.character(response))
}

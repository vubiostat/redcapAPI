#' @describeIn userMethods Remove users from a project.
#' @order 3
#' @export

deleteUsers <- function(rcon, 
                        users, 
                        ...){
  UseMethod("deleteUsers")
}

#' @rdname userMethods
#' @order 6
#' @export

deleteUsers.redcapApiConnection <- function(rcon, 
                                            users, 
                                            ..., 
                                            config = list(), 
                                            api_param = list()){
  ###################################################################
  # Argument Validation                                          ####
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = c("redcapApiConnection"), 
                          add = coll)
  
  checkmate::assert_character(x = users, 
                              min.len = 1, 
                              null.ok = FALSE, 
                              any.missing = FALSE,
                              add = coll)

  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         names = "named", 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  User <- rcon$users()
  
  checkmate::assert_subset(x = users, 
                           choices = User$username, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Build the body list                                          ####
  
  body <- c(list(content = "user", 
                 action = "delete"),
            vectorToApiBodyList(users, "users"))
            
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Make the API Call                                            ####
  rcon$flush_users()
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  invisible(as.character(response))
}

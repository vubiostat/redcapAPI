#' @describeIn userRoleMethods Import user roles to a project.
#' @order 2
#' @export

importUserRoles <- function(rcon, 
                            data, 
                            ...){
  UseMethod("importUserRoles")
}

#' @rdname userRoleMethods
#' @order 5
#' @export

importUserRoles.redcapApiConnection <- function(rcon, 
                                                data,
                                                consolidate = TRUE,
                                                refresh = TRUE, 
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
  
  checkmate::assert_logical(x = consolidate, 
                            len = 1, 
                            null.ok = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = refresh, 
                            len = 1, 
                            null.ok = FALSE, 
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
                           choices = names(REDCAP_USER_ROLE_STRUCTURE), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  data <- prepUserImportData(data,
                             rcon = rcon,
                             consolidate = consolidate, 
                             user_role = TRUE)
  
  ###################################################################
  # Build the body list                                          ####
  
  body <- list(content = "userRole", 
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
  
  if (refresh){
    rcon$refresh_user_roles()
  }
  
  message(sprintf("User Roles Added/Modified: %s", as.character(response)))
}

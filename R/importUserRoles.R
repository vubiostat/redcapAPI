#' @name importUserRoles
#' @title Import User Roles for a REDCap Project
#'
#' @description This method allows you to import new user roles into a 
#'   project while setting their privileges, or update the privileges of 
#'   existing user roles in the project.
#'   
#' @param rcon A \code{redcapConnection} object.
#' @param data A \code{data.frame} with the User Roles to import.
#' @param consolidate \code{logical(1)} If \code{TRUE}, the form and data 
#'   export access values will be read from the expanded columns. Otherwise, 
#'   the consolidated values (as provided by the API export) are utilized.
#' @param ... Arguments to be passed to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'   
#' @details When setting permissions for a user right project access fields (those
#'   not related to forms or exports) are set as either 0 or 1 (or "No Access"
#'   and "Access", respectively). The settings may be any of these four values.
#'   
#' For form access fields, the values must be one of 0, 2, 1, or 3 ("No Access", 
#' "Read Only", "View records/responses and edit records (survey responses are read-only)", 
#' or "Edit survey responses", respectively). 
#' 
#' Data export fields must be one of 0, 2, 3, or 1 ("No Access", "De-Identified", 
#' "Remove Identifier Fields", "Full Data Set", respectively). 
#' 
#' The user data is passed through \code{prepUserImportData} before sending it
#' to the API, so text values listed above may also be used and will be 
#' converted to the numeric equivalent. 
#' 
#' It is also permissible to use a column for each form individually, as can
#' be exported via \code{exportUsers}. With \code{consolidate = TRUE}, these 
#' settings will be consolidated into the text string expected by the API.
#' 
#' @export

importUserRoles <- function(rcon, 
                            data, 
                            ...){
  UseMethod("importUserRoles")
}

#' @rdname importUserRoles
#' @export

importUserRoles.redcapApiConnection <- function(rcon, 
                                                data,
                                                consolidate = TRUE,
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
                               names = "named", 
                               add = coll)
  
  checkmate::assert_logical(x = consolidate, 
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
                             consolidate = consolidate)
  
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
  
  message(sprintf("User Roles Added/Modified: %s", as.character(response)))
}

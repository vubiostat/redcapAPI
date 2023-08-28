#' @name renameRecord
#' @title Rename a Record in a Project
#' 
#' @description This method allows you to rename a record from a project 
#'   in a single API request.
#'   
#' @param rcon A \code{redcapConnection} object. 
#' @param record_name \code{character(1)} The name of an existing record 
#'   in the project. Will also accept \code{numeric(1)} (which will be
#'   coerced to a character).
#' @param new_record_name \code{character(1)} The new name to give to the 
#'   record. Will also accept \code{numeric(1)} (which will be coerced to 
#'   a character).
#' @param arm \code{character(1)} or \code{NULL}, an optional arm number. 
#'   If \code{NULL}, then all records with same name across all arms on 
#'   which it exists (if longitudinal with multiple arms) will be 
#'   renamed to new record name, otherwise it will rename the record 
#'   only in the specified arm.
#' @param ... Arguments to pass to other methods.
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

renameRecord <- function(rcon,
                         record_name, 
                         new_record_name, 
                         arm = NULL, 
                         ...){
  UseMethod("renameRecord")
}

#' @rdname renameRecord
#' @export

renameRecord.redcapApiConnection <- function(rcon, 
                                             record_name, 
                                             new_record_name, 
                                             arm = NULL, 
                                             ..., 
                                             error_handling = getOption("redcap_error_handling"), 
                                             config         = list(), 
                                             api_param      = list()){
  
  if (is.numeric(record_name)) record_name <- as.character(record_name)
  if (is.numeric(new_record_name)) new_record_name <- as.character(new_record_name)
  if (is.numeric(arm)) arm <- as.character(arm)
  
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapConnection", 
                          add = coll)
  
  checkmate::assert_character(x = record_name, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_character(x = new_record_name, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_character(x = arm, 
                              len = 1, 
                              null.ok = TRUE, 
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
  # API Body List                                                ####
  
  body <- list(content = "record", 
               action = "rename", 
               record = record_name, 
               new_record_name = new_record_name, 
               arm = arm)
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API                                                 ####
  
  response <- makeApiCall(rcon, 
                          body = c(body, 
                                   api_param), 
                          config = config)
  
  if (response$status_code != 200){
    redcapError(response, 
                error_handling = error_handling)
  }
  
  as.character(response) == "1"
}

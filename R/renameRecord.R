#' @describeIn recordsManagementMethods Rename an existing record.
#' @order 2
#' @export

renameRecord <- function(rcon,
                         record_name, 
                         new_record_name, 
                         arm = NULL, 
                         ...){
  UseMethod("renameRecord")
}

#' @rdname recordsManagementMethods
#' @order 4
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
  
  invisible(as.character(response) == "1")
}

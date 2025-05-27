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
                                             ...){
  
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
                              add = coll)
  
  checkmate::assert_character(x = new_record_name, 
                              len = length(record_name), 
                              add = coll)
  
  checkmate::assert_character(x = arm, 
                              len = length(record_name), 
                              null.ok = TRUE, 
                              add = coll)

  checkmate::reportAssertions(coll)
  
  
  ###################################################################
  # Rename the records

  results <- 
    mapply(.renameRecordApiCall, 
           record_name = record_name, 
           new_record_name = new_record_name, 
           arm = if (is.null(arm)) lapply(record_name, function(r) NULL)
                 else arm,
           MoreArgs = list(rcon = rcon),
           ...,
           SIMPLIFY = TRUE, 
           USE.NAMES = FALSE)
  
  invisible(results)
}


#####################################################################
# Unexported                                                     ####

.renameRecordApiCall <- function(rcon, 
                                 record_name, 
                                 new_record_name, 
                                 arm, 
                                 ...){
  ###################################################################
  # API Body List                                                ####

  body <- list(content = "record", 
               action = "rename", 
               record = record_name, 
               new_record_name = new_record_name, 
               arm = arm)
  
  ###################################################################
  # Call the API                                                 ####
  invisible('1' == as.character(makeApiCall(rcon, body, ...)))
}

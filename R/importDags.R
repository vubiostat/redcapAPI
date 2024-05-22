#' @describeIn dagMethods Import Data Access Groups to a project.
#' @order 2
#' @export

importDags <- function(rcon, 
                       data, 
                       ...){
  UseMethod("importDags")
}

#' @rdname dagMethods
#' @order 5
#' @export

importDags.redcapApiConnection <- function(rcon, 
                                           data,
                                           ...)
{
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = data, 
                               col.names = "named", 
                               add = coll)

  checkmate::reportAssertions(coll)
  
  checkmate::assert_subset(names(data), 
                           choices = c("data_access_group_name", 
                                       "unique_group_name", 
                                       "data_access_group_id"), 
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

  ###################################################################
  # Call the API                                                 ####
  rcon$flush_dags()
  invisible(as.character(
    makeApiCall(rcon, body, ...)
  ))
}

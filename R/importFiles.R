#' @describeIn fileMethods Import a file to a REDCap project.
#' @order 2
#' @export

importFiles <- function(rcon, 
                        file, 
                        record, 
                        field, 
                        event, 
                        overwrite       = TRUE,
                        repeat_instance = NULL, 
                        ...){
  UseMethod("importFiles")
}

#' @rdname fileMethods
#' @order 5
#' @export

importFiles.redcapApiConnection <- function(rcon, 
                                            file, 
                                            record, 
                                            field, 
                                            event           = NULL, 
                                            overwrite       = TRUE,
                                            repeat_instance = NULL, 
                                            ...)
{
  if (is.numeric(record)) record <- as.character(record)
  
   ##################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_character(x = file, 
                              len = 1, 
                              any.missing = FALSE,
                              add = coll)
  
  checkmate::assert_character(x = record, 
                              len = 1, 
                              any.missing = FALSE, 
                              add = coll)
  
  checkmate::assert_character(x = field, 
                              len = 1, 
                              any.missing = FALSE, 
                              add = coll)
  
  checkmate::assert_character(x = event, 
                              len = 1, 
                              any.missing = FALSE, 
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_logical(x = overwrite, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_integerish(x = repeat_instance,
                               len = 1,
                               any.missing = FALSE,
                               null.ok = TRUE,
                               add = coll)

  checkmate::reportAssertions(coll)
  
  checkmate::assert_file_exists(x = file,
                                add = coll)
  
  # make sure 'field' exists in the project and are 'file' fields
  MetaData <- rcon$metadata()

  if (!field %in% MetaData$field_name) 
    coll$push(paste("'", field, "' does not exist in the project.", sep=""))
  
  if (MetaData$field_type[MetaData$field_name == field] != "file")
    coll$push(paste0("'", field, "' is not of field type 'file'"))
  
  # make sure 'event' exists in the project
  
  is_project_longitudinal <- as.logical(rcon$projectInformation()$is_longitudinal)

  if (is_project_longitudinal)
  {
    EventsList <- rcon$events()
    
    if (nrow(EventsList) == 0)
    {
      message("No events defined in this project. Ignoring the 'event' argument.")
      event <- NULL
    } else {
      checkmate::assert_subset(x = event, 
                               choices = EventsList$unique_event_name, 
                               add = coll)
      checkmate::reportAssertions(coll)
    }
  } else {
    event <- NULL
  }
  
  if (!overwrite)
  {
    fileThere <- exportRecordsTyped(rcon, 
                                    records = record, 
                                    fields = c(MetaData$field_name[1], field), 
                                    events = event)
    if (nrow(fileThere) > 0 && length(repeat_instance) > 0){
      fileThere <- fileThere[fileThere$redcap_repeat_instance %in% repeat_instance, ]
    }
    
    if (nrow(fileThere) > 0 && !any(is.na(fileThere[[field]])))
      coll$push("A file exists and overwrite = FALSE")
  }
  
  checkmate::reportAssertions(coll)
  
   ###########################################################################
  # Build the body list
  
  body <- list(content = 'file',
               action = 'import', 
               record = record,
               field = field, 
               file = .curlUploadFile(file),
               returnFormat = 'csv', 
               event = event, 
               repeat_instance = repeat_instance)

   ###########################################################################
  # Make the API Call
  response <- makeApiCall(rcon, body, ...)

  invisible(response$status_code == 200L)
}

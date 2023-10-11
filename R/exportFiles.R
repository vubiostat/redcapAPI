#' @describeIn fileMethods Export a file from a REDCap project.
#' @order 1
#' @export

exportFiles <- function(rcon, 
                        record, 
                        field, 
                        event      = NULL, 
                        dir, 
                        file_prefix = TRUE, 
                        ...){
  UseMethod("exportFiles")
}

#' @rdname fileMethods
#' @order 4
#' @export

exportFiles.redcapApiConnection <- function(rcon, 
                                            record, 
                                            field, 
                                            event           = NULL, 
                                            dir, 
                                            file_prefix      = TRUE, 
                                            repeat_instance = NULL,
                                            ...,
                                            error_handling  = getOption("redcap_error_handling"),
                                            config          = list(), 
                                            api_param       = list()){

  if (is.numeric(record)) record <- as.character(record)
  
  dots <- list(...)
  if ("filePrefix" %in% names(dots)) file_prefix <- dots$filePrefix
  
   ###########################################################################
  # Check parameters passed to function
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
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
  
  checkmate::assert_character(x = dir, 
                              len = 1, 
                              any.missing = FALSE, 
                              add = coll)
  
  checkmate::assert_logical(x = file_prefix,
                            len = 1,
                            add = coll)
  
  checkmate::assert_integerish(x = repeat_instance, 
                               len = 1, 
                               any.missing = FALSE,
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
 
  checkmate::assert_directory_exists(x = dir, 
                                     add = coll)

  # Secure the MetaData
  MetaData <- rcon$metadata()
  
  checkmate::assert_subset(x = field, 
                           choices = MetaData$field_name, 
                           add = coll)
  
  # make sure 'field' exist in the project and are 'file' fields
  if (!isTRUE(MetaData$field_type[MetaData$field_name == field] == "file"))
  {
    coll$push(paste0("'", field, "' is not of field type 'file'"))
  }
  
  # Check that event exists in the project
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
    }
  } else {
    event <- NULL
  }
  
  checkmate::reportAssertions(coll)
  
   ###########################################################################
  # Build the Body List
  body <- list(content = 'file',
               action = 'export', 
               returnFormat = 'csv',
               record = record,
               field = field, 
               event = event, 
               repeat_instance = repeat_instance)
  
  body <- body[lengths(body) > 0]
  
   ###########################################################################
  # Make the API Call
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)

  if (response$status_code != 200) redcapError(response, error_handling)
  
  prefix <- 
    if (file_prefix) sprintf("%s%s%s", 
                            record,
                            if (is.null(event)) "" else "-",
                            if (is.null(event)) "" else event) else ""
  
  file_saved <- reconstituteFileFromExport(response = response, 
                                           dir = dir, 
                                           dir_create = FALSE, 
                                           file_prefix = prefix)
  
  invisible(file.path(file_saved$directory, 
                      file_saved$filename))
}

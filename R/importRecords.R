#' @name importRecords
#' @title Import Records to a Project
#'
#' @description These methods enable the user to import new records or update
#'   existing records to a project. 
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @inheritParams common-cast-args
#' @param data `data.frame`. The data to be imported to the project.
#' @param overwrite_behavior `character(1)`. One of `c("normal", "overwrite")`. 
#'   `"normal"` prevents blank fields from overwriting populated fields.  
#'   `"overwrite"` causes blanks to overwrite data in the database.
#' @param force_auto_number `logical(1)`. If record auto-numbering has been
#'   enabled in the project, it may be desirable to import records where each 
#'   record's record name is automatically determined by REDCap (just as it 
#'   does in the user interface). When `TRUE`, the 
#'   record names provided in the request will not be used (although they 
#'   are still required in order to associate multiple rows of data to an 
#'   individual record in the request); instead those records in the 
#'   request will receive new record names during the import process. 
#'   It is recommended that the user use `return_content = "auto_ids"`
#'   when `force_auto_number = TRUE`
#' @param return_content `character(1)`.  
#'   One of `c("count", "ids", "nothing", "auto_ids")`.
#'   'count' returns the number of records imported; 
#'   'ids' returns the record ids that are imported;
#'   'nothing' returns no message; 
#'   'auto_ids' returns a list of pairs of all record IDs that were imported. 
#'   If used when `force_auto_number = FALSE`, the value will be changed to `'ids'`.
#' @param skip_import `logical(1)`. When `TRUE`, the data will undergo validation
#'   and casting, but will not be sent to the project. This permits the user 
#'   to obtain and review the data before attempting to import it.
#' @param batch_size `integerish(1)`.  Specifies the number of subjects to be included 
#'   in each batch of a batched export or import.  Non-positive numbers 
#'   export/import the entire operation in a single batch. 
#'   Batching may be beneficial to prevent tying up smaller servers.  
#'   See Details.
#'
#' @details
#' A 'batched' import is one where the export is performed over a series of 
#' API calls rather than one large call.  For large projects on small servers, 
#' this may prevent a single user from tying up the server and forcing others 
#' to wait on a larger job. 
#' 
#' ## BioPortal Fields
#' 
#' Text fields that are validation enabled using the BioPortal Ontology service
#' may be imported by providing the coded value. Importing the coded value 
#' does not, however, guarantee that the labeled value will be immediately
#' available. Labels for BioPortal values are cached on the REDCap server
#' in a process that occurs when viewing data in the user interface. Thus, 
#' if the label has not be previously cached on the server, the code will be
#' used to represent both the code and the label.
#' 
#' @return
#' Returns a data frame with the data after having been passed through
#' `castForImport`. 
#' 
#' The data frame has an attribute `return_content` that gives the content
#' returned by the API.
#'
#' @seealso 
#' [exportRecords()], \cr
#' [deleteRecords()], \cr
#' [exportRecordsTyped()], \cr
#' [castForImport()]
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#' 
#' # Import records
#' NewData <- data.frame(record_id = c(1, 2, 3), 
#'                       age = c(27, 43, 32), 
#'                       date_of_visit = rep(Sys.Date(), 3))
#' importRecords(rcon, 
#'               data = NewData)
#'               
#'               
#' # Import records using auto numbering
#' NewData <- data.frame(record_id = c(1, 2, 3), 
#'                       age = c(27, 43, 32), 
#'                       date_of_visit = rep(Sys.Date(), 3))
#' importRecords(rcon, 
#'               data = NewData,
#'               force_auto_number = TRUE)
#'               
#' 
#' } 
#' 
#' @export

importRecords <- function(rcon, 
                          data,
                          overwrite_behavior = c('normal', 'overwrite'),
                          return_content     = c('count', 'ids', 'nothing', 'auto_ids'),
                          force_auto_number  = FALSE,
                          ...){
  UseMethod("importRecords")
}

#' @rdname importRecords
#' @export

importRecords.redcapApiConnection <- function(rcon, 
                                              data,
                                              overwrite_behavior = c('normal', 'overwrite'),
                                              return_content     = c('count', 'ids', 'nothing', 'auto_ids'),
                                              force_auto_number  = FALSE,
                                              ...,
<<<<<<< HEAD
                                              na                 = list(), 
                                              validation         = list(), 
                                              cast               = list(),
                                              skip_import        = FALSE,
                                              batch_size         = NULL,
                                              error_handling     = getOption("redcap_error_handling"), 
                                              config             = list(), 
                                              api_param          = list()){
  
  dots <- list(...)
  
  if ("overwriteBehavior" %in% names(dots)) overwrite_behavior <- dots$overwriteBehavior
  if ("returnContent" %in% names(dots)) return_content <- dots$returnContent
  if ("batch.size" %in% names(dots)) batch_size <- dots$batch.size
=======
                                              batch.size        = -1,
                                              error_handling = getOption("redcap_error_handling"), 
                                              config = list(), 
                                              api_param = list())
{
  if(is.null(attr(data, "castForImport")))
    message("importRecords will change how it validates data in version 3.0.0.\n",
            "We recommend preparing your data for import using castForImport .")
>>>>>>> main
  
   ##################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = data, 
                               add = coll)
  
  overwrite_behavior <- 
    checkmate::matchArg(x = overwrite_behavior, 
                        choices = c('normal', 'overwrite'),
                        .var.name = "overwrite_behavior",
                        add = coll)
  
  return_content <- 
    checkmate::matchArg(x = return_content, 
                        choices = c('count', 'ids', 'nothing', 'auto_ids'),
                        .var.name = "return_content",
                        add = coll)
  
  checkmate::assert_logical(x = force_auto_number, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_logical(x = skip_import, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_integerish(x = batch_size,
                               lower = 1,
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
  
  # Remove survey identifiers
  data <- .importRecords_removeSurveyIdentifiers(data, rcon)
  
  # Validate Field Names
  data <- .importRecords_validateFieldNames(data, rcon)
  
  # Ensure record identifier is present and in proper position
  data <- .importRecords_positionRecordIdentifier(data, rcon, coll)
  
  # Remove Calculated Fields
  data <- .importRecords_removeCalculatedFields(data, rcon)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Cast for Import and Import Records                           ####
  
  if (!force_auto_number && return_content == 'auto_ids'){
    return_content = 'ids'
  }

  data <- castForImport(data, 
                        rcon, 
                        fields = names(data), 
                        na = na, 
                        validation = validation, 
                        cast = cast)
  
  if (!skip_import){
    body <- list(content = "record", 
                 format = "csv", 
                 type = "flat", 
                 overwriteBehavior = overwrite_behavior, 
                 forceAutoNumber = force_auto_number, 
                 returnContent = return_content, 
                 returnFormat = "csv")

    responses <- .importRecords_makeImportCall(body = body, 
                                               rcon = rcon, 
                                               data = data,
                                               return_content = return_content,
                                               batch_size = batch_size,
                                               error_handling = error_handling, 
                                               api_param = api_param, 
                                               config = config)

    switch(return_content, 
           "count" = message(sprintf("Records imported: %s", responses)), 
           "nothing" = NULL,
           message(sprintf("Records imported: %s", nrow(responses))))
    
    attr(data, "return_content") <- responses
  }
  invisible(data)
}

#####################################################################
# Unexported 

.importRecords_removeSurveyIdentifiers <- function(data, rcon){
  field_to_remove <- 
    c("redcap_survey_identifier",
      sprintf("%s_timestamp", 
              rcon$instruments()$instrument_name))
  
  data[!names(data) %in% field_to_remove]
}

.importRecords_validateFieldNames <- function(data, rcon){
  with_complete_fields <- rcon$fieldnames()$export_field_name
  
  # Remove survey identifiers and data access group fields from data
  w.remove <- 
    which(names(data) %in% 
            c("redcap_survey_identifier",
              paste0(unique(MetaData$form_name), "_timestamp")))
  if (length(w.remove) > 0) data <- data[-w.remove]
  
  mchoices <- which(vapply(data, inherits, logical(1), 'mChoice'))
  if(length(mchoices) > 0)
  {
    coll$push(paste0(
      "The variable(s) ", 
      paste0(names(data)[mchoices], collapse=", "), 
      " are mChoice formatted and cannot be imported."))
  }
  
  # Validate field names
  unrecognized_names <- !(names(data) %in% c(with_complete_fields, REDCAP_SYSTEM_FIELDS))
  if (any(unrecognized_names))
  {
    coll$push(paste0(
      "The variable(s) ", 
      paste0(names(data)[unrecognized_names], collapse=", "), 
      " are not found in the project and/or cannot be imported."))
  }
  
  data
}

.importRecords_positionRecordIdentifier <- function(data, rcon, coll){
  MetaData <- rcon$metadata()
  # Check that the study id exists in data
  if (!MetaData$field_name[1] %in% names(data))
  {
    coll$push(paste0("The variable '", 
                     MetaData$field_name[1], 
                     "' cannot be found in 'data'. ",
                     "Please include this variable and place it in the first column."))
  }
  
  # If the study id is not in the the first column, move it and print a warning
  if (MetaData$field_name[1] %in% names(data) && 
      MetaData$field_name[1] != names(data)[1])
  {
    message("The variable'", MetaData$field_name[1], 
            "' was not in the first column. ",
            "It has been moved to the first column.")
    w <- which(names(data) == MetaData$field_name[1])
    data <- data[c(w, (1:length(data))[-w])]
  }
  
  data
}

.importRecords_removeCalculatedFields <- function(data, rcon){
  MetaData <- rcon$metadata()
  
  calc_field <- MetaData$field_name[MetaData$field_type == "calc"]
  calc_field <- calc_field[calc_field %in% names(data)]
  
  if (length(calc_field) > 0)
  {
    message("The variable(s) '", 
            paste(calc_field, collapse="', '"),
            "' are calculated fields and cannot be imported. ",
            "They have been removed from the imported data frame.")
    data <- data[!names(data) %in% calc_field]
  }
  
  data
}

.importRecords_makeImportCall <- function(body, 
                                          rcon, 
                                          data, 
                                          return_content,
                                          batch_size, 
                                          error_handling, 
                                          api_param, 
                                          config){
  # Batch the data 
  if (length(batch_size) == 0){
    data_list <- list(data)
  } else {
    data_list <- split(data, (seq(nrow(data))-1) %/% batch_size) 
  }
  
  # Collect responses
  responses <- vector("list", length(data_list))
  
  for (i in seq_along(data_list)){
    this_body <- c(body, 
                   list(data = writeDataForImport(data_list[[i]])))
    this_body <- this_body[lengths(this_body) > 0]
 
    responses[[i]] <- makeApiCall(rcon, 
                                  body = c(this_body, api_param), 
                                  config = config)
    
    if (responses[[i]]$status_code != 200){
      return(redcapError(responses[[i]], error_handling))
    } 
  }
  
  response_char <- vapply(responses, 
                          function(x) as.character(x), 
                          character(1))
  
  switch(return_content, 
         "count" = sum(vapply(response_char, as.numeric, numeric(1))), 
         "nothing" = NULL, 
         do.call("rbind", lapply(response_char, 
                                 function(rc) utils::read.csv(text = rc, 
                                                              stringsAsFactors = FALSE))))
}


import_records_unbatched <- function(rcon, 
                                     data, 
                                     overwriteBehavior,
                                     returnContent, 
                                     force_auto_number,
                                     config, 
                                     api_param)
{
  out <- data_frame_to_string(data)
  
  ## Reattach attributes
  attributes(out) <- 
    list("Content-Type" = structure(c("text/html", "utf-8"),
                                    .Names = c("", "charset")))
  
   ##################################################################
  # Make API Body List
  
  body <- list(token = rcon$token, 
               content = 'record', 
               format = 'csv',
               type = 'flat', 
               overwriteBehavior = overwriteBehavior,
               returnContent = returnContent,
               returnFormat = 'csv', 
               forceAutoNumber = tolower(force_auto_number),
               data = out)
  
  body <- body[lengths(body) > 0]
  
   ##################################################################
  # Call the API
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code == "200"){
    if (returnContent %in% c("ids", "auto_ids")){
      as.data.frame(response)
    } else {
      as.character(response)
    }
  }
  else 
    redcapError(response, error_handling = "error")
}

#####################################################################
# Unexported

data_frame_to_string <- function(data)
{
  paste0(
    utils::capture.output(
      utils::write.table(data, 
                         sep = ",",
                         col.names = TRUE,
                         row.names = FALSE,
                         na = "")
    ),
    collapse = "\n"
  )
}

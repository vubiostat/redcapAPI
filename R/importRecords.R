#' @name importRecords
#' @title Import Records to a REDCap Database
#' 
#' @description Imports records from a \code{data.frame} to a REDCap Database
#'
#' @param rcon A REDCap connection object as created by \code{redcapConnection}.
#' @param data A \code{data.frame} to be imported to the REDCap project.
#' @param overwriteBehavior Character string.  'normal' prevents blank
#'   fields from overwriting populated fields.  'overwrite' causes blanks to
#'   overwrite data in the REDCap database.
#' @param returnContent Character string.  'count' returns the number of
#'   records imported; 'ids' returns the record ids that are imported;
#'   'nothing' returns no message; 'auto_ids' returns a list of pairs of all 
#'   record IDs that were imported. If used when \code{force_auto_number = FALSE}, 
#'   the value will be changed to \code{'ids'}.
#' @param returnData Logical.  Prevents the REDCap import and instead
#'   returns the data frame that would have been given
#'   for import. This is sometimes helpful if the API import fails without
#'   providing an informative message. The data frame can be written to a csv
#'   and uploaded using the interactive tools to troubleshoot the
#'   problem. Please send us an e-mail if you find errors I haven't
#'   accounted for.
#' @param logfile An optional filepath (preferably .txt) in which to print the
#'   log of errors and warnings about the data.
#'   If \code{""}, the log is printed to the console.
#' @param force_auto_number \code{logical(1)} If record auto-numbering has been
#'   enabled in the project, it may be desirable to import records where each 
#'   record's record name is automatically determined by REDCap (just as it 
#'   does in the user interface). If this parameter is set to 'true', the 
#'   record names provided in the request will not be used (although they 
#'   are still required in order to associate multiple rows of data to an 
#'   individual record in the request), but instead those records in the 
#'   request will receive new record names during the import process. 
#'   NOTE: To see how the provided record names get translated into new auto
#'    record names, the returnContent parameter should be set to 'auto_ids', 
#'    which will return a record list similar to 'ids' value, but it will have
#'    the new record name followed by the provided record name in the request, 
#'    in which the two are comma-delimited.
#' @param batch.size Specifies size of batches.  A negative value
#'   indicates no batching.
#' @param ... Arguments to be passed to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcapError}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'
#' @details
#' A record of imports through the API is recorded in the Logging section
#' of the project.
#'
#' \code{importRecords} prevents the most common import errors by testing the
#' data before attempting the import.  Namely
#' \enumerate{
#'   \item Check that all variables in \code{data} exist in the REDCap data dictionary.
#'   \item Check that the study id variable exists
#'   \item Force the study id variable to the first position in the data frame (with a warning)
#'   \item Remove calculated fields (with a warning)
#'   \item Verify that REDCap date fields are represented in the data frame as
#'     either character, POSIXct, or Date class objects.
#'   \item Determine if values are within their specified validation limits.
#' }
#'
#' See the documentation for \code{\link{validateImport}} for detailed
#' explanations of the validation.
#' 
#' @section Limitations:
#' 
#' The REDCap API is fairly restrictive about what it will accept as valid data for import. 
#' \code{importRecords} tries to simplify the process by allowing users to 
#' submit data in any form recognized by the data dictionary. It is then converted
#' internally to the 
#' appropriate text format for import. This means, for example, that a radio button value
#' where the code \code{1} is mapped to the label \code{Guitar} (defined in the user interface
#' with "1, Guitar"), the user can provide
#' either "1" or "Guitar" as a value and \code{importRecords} will translate it to the 
#' code that the API expects. 
#' 
#' While this provides a level of convenience for the user, it has some limitations when
#' applied to checkbox values. When submitting checkbox values for import, it is strongly 
#' recommended that you submit either the code "0" (for unchecked), "1" (for checked), or the 
#' labels "Unchecked" and "Checked". 
#' 
#' In particular, when the checkbox labels are defined with a code or label that is "0" or "1"
#' (for example, "0, checkbox_label" or "check_code, 0"), \code{importRecords} is unable to 
#' determine if a 0 indicates an unchecked box or if the zero is the label of a checked box. 
#' When encountering ambiguity, \code{importRecords} will always assume "0" represents an
#' unchecked box and "1" represents a checked box.
#'
#' @author Benjamin Nutter\cr
#' with thanks to Josh O'Brien and etb (see references)
#'
#' @references
#' See the REDCap API documentation at your institution's REDCap documentation.
#'
#' @seealso \code{\link{validateImport}}
#'
#' @export

importRecords <- function(rcon, 
                          data,
                          overwriteBehavior = c('normal', 'overwrite'),
                          returnContent     = c('count', 'ids', 'nothing', 'auto_ids'),
                          returnData        = FALSE, 
                          logfile           = "", 
                          ...){
  UseMethod("importRecords")
}

#' @rdname importRecords
#' @export

importRecords.redcapApiConnection <- function(rcon, 
                                              data,
                                              overwrite_behavior = c('normal', 'overwrite'),
                                              return_content     = c('count', 'ids', 'nothing', 'auto_ids'),
                                              logfile            = "", 
                                              force_auto_number  = FALSE,
                                              ...,
                                              batch_size         = -1,
                                              error_handling     = getOption("redcap_error_handling"), 
                                              config             = list(), 
                                              api_param          = list()){
  
  dots <- list(...)
  
  if ("overwriteBehavior" %in% names(dots)) overwrite_behavior <- dots$overwriteBehavior
  if ("returnContent" %in% names(dots)) return_content <- dots$returnContent
  if ("batch.size" %in% names(dots)) batch_size <- dots$batch.size
  
   ##################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = data, 
                               add = coll)
  
  overwriteBehavior <- 
    checkmate::matchArg(x = overwrite_behavior, 
                        choices = c('normal', 'overwrite'),
                        .var.name = "overwriteBehavior",
                        add = coll)
  
  returnContent <- 
    checkmate::matchArg(x = return_content, 
                        choices = c('count', 'ids', 'nothing', 'auto_ids'),
                        .var.name = "returnContent",
                        add = coll)
  
  checkmate::assert_character(x = logfile,
                              len = 1,
                              add = coll)
  
  checkmate::assert_logical(x = force_auto_number, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_integerish(x = batch_size,
                               len = 1,
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

  
  if (!force_auto_number && return_content == 'auto_ids'){
    return_content = 'ids'
  }
  
  ###################################################################
  # Cast for Import and Import Records                           ####
  
  data <- castForImport(data, rcon, fields = names(data))
  
  body <- list(content = "record", 
               format = "csv", 
               type = "flat", 
               overwriteBehavior = overwrite_behavior, 
               forceAutoNumber = force_auto_number, 
               returnContent = return_content, 
               returnFormat = "csv")
  
  responses <- .importRecords_makeImportCall(body = body, 
                                             rcon = rcon, 
                                             batch_size = batch_size, 
                                             api_param = api_param, 
                                             config = config)

  ###################################################################
  # Wrap up                                                      ####
  
  switch(return_content, 
         "count" = message(sprintf("Records imported: %s", responses)), 
         message(sprintf("IDs imported: %s", responses)))
  
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
  
  unrecognized_names <- !(names(data) %in% c(with_complete_fields, REDCAP_SYSTEM_FIELDS))
  
  if (any(unrecognized_names))
  {
    message("The variable(s) ", 
            paste0(names(data)[unrecognized_names], collapse=", "), 
            " are not found in the project and/or cannot be imported. They have been removed from the imported data frame.")
    data[!unrecognized_names]
  }
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
                                          batch_size, 
                                          api_param, 
                                          config){
  if (length(batch_size) == 0){
    data_list <- list(data)
  } else {
    data_list <- split(data, (seq(nrow(data))-1) %/% batch_size) 
  }
  
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
  
  x <- vapply(responses,
              function(x) as.character(x), 
              character(1))
  
  paste0(x, collapse = ", ")
}

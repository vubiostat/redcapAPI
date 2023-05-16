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
#'   for import.  This is sometimes helpful if the API import fails without
#'   providing an informative message. The data frame can be written to a csv
#'   and uploaded using the interactive tools to troubleshoot the
#'   problem.  Please shoot me an e-mail if you find errors I havne't
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
#'   see \code{\link{redcap_error}}
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
                                              overwriteBehavior = c('normal', 'overwrite'),
                                              returnContent     = c('count', 'ids', 'nothing', 'auto_ids'),
                                              returnData        = FALSE, 
                                              logfile           = "", 
                                              force_auto_number = FALSE,
                                              ...,
                                              batch.size        = -1,
                                              error_handling = getOption("redcap_error_handling"), 
                                              config = list(), 
                                              api_param = list()){
  
   ##################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = data, 
                               add = coll)
  
  overwriteBehavior <- 
    checkmate::matchArg(x = overwriteBehavior, 
                        choices = c('normal', 'overwrite'),
                        .var.name = "overwriteBehavior",
                        add = coll)
  
  returnContent <- 
    checkmate::matchArg(x = returnContent, 
                        choices = c('count', 'ids', 'nothing', 'auto_ids'),
                        .var.name = "returnContent",
                        add = coll)
  
  checkmate::assert_logical(x = returnData,
                            len = 1,
                            add = coll)
  
  checkmate::assert_character(x = logfile,
                              len = 1,
                              add = coll)
  
  checkmate::assert_logical(x = force_auto_number, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_integerish(x = batch.size,
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
  
  
  MetaData <- rcon$metadata()

  version <- rcon$version()

  with_complete_fields <- rcon$fieldnames()$export_field_name
  
  # Remove survey identifiers and data access group fields from data
  w.remove <- 
    which(names(data) %in% 
            c("redcap_survey_identifier",
              paste0(unique(MetaData$form_name), "_timestamp")))
  if (length(w.remove)) data <- data[-w.remove]
  
  # Validate field names
  unrecognized_names <- !(names(data) %in% c(with_complete_fields, REDCAP_SYSTEM_FIELDS))

  if (any(unrecognized_names))
  {
    message("The variable(s) ", 
            paste0(names(data)[unrecognized_names], collapse=", "), 
            " are not found in the project and/or cannot be imported. They have been removed from the imported data frame.")
    data <- data[!unrecognized_names]
  }
  
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
  
  # Confirm that date fields are either character, Date class, or POSIXct
  date_vars <- MetaData$field_name[grepl("date_", MetaData$text_validation_type_or_show_slider_number)]
  
  if (any(date_vars %in% names(data))){
    date_vars <- date_vars[date_vars %in% names(data)]
    bad_date_fmt <- 
      !vapply(X = data[date_vars], 
              FUN = function(x) is.character(x) | "Date" %in% class(x) | "POSIXct" %in% class(x),
              FUN.VALUE = logical(1))
    
    if (any(bad_date_fmt))
    {
      coll$push(paste0("The variables '", 
                       paste(date_vars[bad_date_fmt], collapse="', '"),
                       "' must have class Date, POSIXct, or character."))
    }
  }
  
  # Remove calculated fields
  calc_field <- MetaData$field_name[MetaData$field_type == "calc"]
  
  if (length(calc_field) > 0)
  {
    message("The variable(s) '", 
            paste(calc_field, collapse="', '"),
            "' are calculated fields and cannot be imported. ",
            "They have been removed from the imported data frame.")
    data <- data[!names(data) %in% calc_field]
  }
  
  checkmate::reportAssertions(coll)
  
  if (!force_auto_number && returnContent == 'auto_ids'){
    returnContent = 'ids'
  }
  
  
  idvars <- 
    if ("redcap_event_name" %in% names(data))
      c(MetaData$field_name[1], "redcap_event_name") 
  else 
    MetaData$field_name[1]
  
  data <- validateImport(data = data,
                         meta_data = MetaData,
                         logfile = logfile)
  
  if (returnData) return(data)
  
  #** Format the data for REDCap import
  #** Thanks go to:
  #**   https://github.com/etb/my-R-code/blob/master/R-pull-and-push-from-and-to-REDCap.R
  #**   http://stackoverflow.com/questions/12393004/parsing-back-to-messy-api-strcuture/12435389#12435389
  
  if (batch.size > 0)
  {
    import_records_batched(rcon = rcon, 
                           data = data,
                           batch.size = batch.size,
                           overwriteBehavior = overwriteBehavior,
                           returnContent = returnContent, 
                           force_auto_number = force_auto_number,
                           config = config, 
                           api_param = api_param)
  }
  else
  {
    import_records_unbatched(rcon = rcon,
                             data = data,
                             overwriteBehavior = overwriteBehavior,
                             returnContent = returnContent, 
                             force_auto_number = force_auto_number,
                             config = config, 
                             api_param = api_param)
  }
}

#####################################################################
## UNEXPORTED FUNCTIONS
#####################################################################

import_records_batched <- function(rcon, 
                                   data, 
                                   batch.size, 
                                   overwriteBehavior,
                                   returnContent, 
                                   force_auto_number,
                                   config, 
                                   api_param)
{
  n.batch <- nrow(data) %/% batch.size + 1
  
  ID <- data.frame(row = 1:nrow(data))
  
  ID$batch.number <- rep(1:n.batch, 
                         each = batch.size, 
                         length.out = nrow(data))
  
  data[is.na(data)] <- ""
  
  data <- split(data, 
                f = ID$batch.number)
  
  out <- lapply(X = data, 
                FUN = data_frame_to_string)
  
  att <- list("Content-Type" = 
                structure(c("text/html", "utf-8"),
                          .Names = c("", "charset")))
  out <- lapply(X = out, 
                FUN = function(d){
                  attributes(d) <- att; 
                  return(d)
                })
  
   ##################################################################
  # Make API Body List
  
  body <- list(token = rcon$token, 
               content = 'record', 
               format = 'csv',
               type = 'flat', 
               overwriteBehavior = overwriteBehavior,
               returnContent = returnContent,
               forceAutoNumber = tolower(force_auto_number),
               returnFormat = 'csv')
  body <- c(body, api_param)
  
  body <- body[lengths(body) > 0]
  
  
   ##################################################################
  # Call the API
  responses <- vector("list", length = length(out))
  
  for (i in seq_along(out))
  {
    responses[[i]] <- makeApiCall(rcon, 
                                  body = c(body, 
                                           list(data = out[[i]])), 
                                  config = config)
  }
  
  if (all(unlist(sapply(X = responses, 
                        FUN = function(y) y["status_code"])) == "200"))
  {
    vapply(responses, as.character, character(1))
  }
  else 
  {
    status.code <- unlist(sapply(X = responses, 
                                 FUN = function(y) y["status_code"]))
    msg <- sapply(responses, as.character)
    
    stop(paste(paste0(status.code[status.code != "200"], 
                      ": ", 
                      msg[status.code != "200"]), 
               collapse="\n"))
  }
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
      read.csv(text = as.character(response), 
               na.strings = "", 
               stringsAsFactors = FALSE)
    } else {
      as.character(response)
    }
  }
  else 
    redcap_error(response, error_handling = "error")
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

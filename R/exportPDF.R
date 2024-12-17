#' @name exportPdf
#' @title Export PDF file of Data Collection Instruments
#' 
#' @description These methods allow the user to download PDF files of 
#'   data collection instruments.  The download may be with or without 
#'   collected data; and may return a single record, multiple records, 
#'   or all records.
#'
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param dir `character(1)`. The directory into which the file 
#'   should be saved.
#' @param filename `character(1)`. The base of the file name.  When 
#'   `record = NULL`, it will be appended with `"_blank.pdf"`.  When
#'   `record` has a value, it will be appended with `"_record_[record id].pdf"` 
#' @param record `character(1)`, `integerish(1)`, or `NULL`.  
#'   The record id for which forms should be downloaded.  
#' @param events `character`. The events for which forms should be downloaded
#' @param instruments `character`. The instruments for which forms 
#'   should be downloaded
#' @param all_records `logical(1)`. When `TRUE` forms for all records 
#'   are downloaded. When `TRUE`, this overrides the `records` argument.
#' 
#' @details
#' These methods mimics the behavior of "Download PDF of Instruments" button on the
#' REDCap user interface. They permit the user to export a PDF file for:
#' 
#'  1. A single collection instrument (blank)c
#'  2. All instruments (blank)
#'  3. A single instrument (with data from a single record)c
#'  4. All instruments (with data from a single record)
#'  5. All instruments (with data from all records)
#'
#'
#' @return 
#' `exportPdf` invisibly returns the location on the local system 
#'   to whihc the files is saved.
#'
#' @seealso
#' [exportMetaData()],\cr
#' [importMetaData()], \cr
#' [exportFieldNames()], \cr
#' [exportInstruments()],\cr
#' [exportMappings()],\cr
#' [importMappings()]
#' 
#' @export

exportPdf <- function(rcon, 
                      dir, 
                      filename    = "redcap_forms_download", 
                      record      = NULL, 
                      events      = NULL, 
                      instruments = NULL, 
                      all_records = FALSE, 
                      ...){
  UseMethod("exportPdf")
}

#' @rdname exportPdf
#' @export

exportPdf.redcapApiConnection <- function(rcon, 
                                          dir, 
                                          filename       = "redcap_forms_download",
                                          record         = NULL, 
                                          events         = NULL,
                                          instruments    = NULL, 
                                          all_records    = FALSE, 
                                          ...)
{
  if (is.numeric(record)) record <- as.character(record)
    
   ##################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  checkmate::assert_character(x = dir, 
                              len = 1, 
                              any.missing = FALSE,
                              add = coll)
  
  checkmate::assert_character(x = filename, 
                              len = 1, 
                              any.missing = FALSE, 
                              add = coll)
  
  checkmate::assert_character(x = record, 
                              len = 1, 
                              any.missing = FALSE,
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_character(x = events,
                              len = 1, 
                              any.missing = FALSE,
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_character(x = instruments,
                              len  = 1,
                              any.missing = FALSE, 
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_logical(x = all_records,
                            len = 1,
                            any.missing = FALSE,
                            add = coll)

  checkmate::reportAssertions(coll)
  
  checkmate::assert_directory_exists(x = dir, 
                                     add = coll)

  
  Events <- rcon$events()
  
  Instruments <- rcon$instruments()
  
  checkmate::assert_subset(x = events, 
                           choices = Events$unique_event_name,
                           add = coll)

  checkmate::assert_subset(x = instruments, 
                           choices = Instruments$instrument_name,
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
   ##################################################################
  # Make the Body List
  
  body <- list(content = 'pdf', 
               returnFormat = 'csv', 
               allRecords = if (all_records) as.numeric(all_records) else NULL, 
               record = record, 
               event = events, 
               instrument = instruments)

   ##################################################################
  # Call the API
  
  response <- makeApiCall(rcon, body, ...)
  filename <- 
    if (all_records)
      paste0(filename, "_all_records.pdf")
    else if (is.null(record)) 
      paste0(filename, "_blank.pdf")
    else 
      paste0(filename, "_record_", record, ".pdf")
  
  reconstituteFileFromExport(response, 
                             dir = dir, 
                             dir_create = FALSE, 
                             file_prefix = "", 
                             filename = filename)
  
  invisible(file.path(dir, filename))
}
  

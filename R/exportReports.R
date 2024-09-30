#' @describeIn recordsMethods Export data via a report.
#' @order 3
#' @export

exportReports <- function(rcon, 
                          report_id, 
                          raw_or_label = "raw", 
                          raw_or_label_headers = "raw",
                          export_checkbox_label = FALSE, 
                          csv_delimiter = ",",
                          ...){
  UseMethod("exportReports")
}

#' @rdname recordsMethods
#' @order 5
#' @export

exportReports.redcapApiConnection <- function(rcon, 
                                              report_id, 
                                              raw_or_label = "raw", 
                                              raw_or_label_headers = "raw",
                                              export_checkbox_label = FALSE, 
                                              csv_delimiter = ",",
                                              ...){
  
  if (!is.numeric(report_id)) report_id <- as.numeric(report_id)
  
  ##################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_integerish(x = report_id,
                               len = 1,
                               any.missing = FALSE,
                               add = coll)
  
  raw_or_label <- checkmate::matchArg(x = raw_or_label, 
                                      choices = c("raw", "label"), 
                                      add = coll, 
                                      .var.name = "raw_or_label")
  
  raw_or_label_headers <- checkmate::matchArg(x = raw_or_label_headers, 
                                              choices = c("raw", "label"), 
                                              add = coll, 
                                              .var.name = "raw_or_label_headers")
  
  checkmate::assert_logical(x = export_checkbox_label, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  csv_delimiter <- checkmate::matchArg(x = csv_delimiter, 
                                       choices = c(",", "\t", ";", "|", "^"), 
                                       add = coll, 
                                       .var.name = "csv_delimiter")
  
  checkmate::reportAssertions(coll)
  
  ##################################################################
  # Get required information
  
  MetaData <- rcon$metadata()
  
  #* for purposes of the export, we do not need the descriptive fields. 
  #* Including them makes the process more error prone, so we'll ignore them.
  MetaData <- MetaData[!MetaData$field_type %in% "descriptive", ]  
  
  version <- rcon$version()
  
  ##################################################################
  # Make API Body List
  
  body <- list(token = rcon$token, 
               content = 'report',
               format = 'csv', 
               returnFormat = 'csv', 
               report_id = report_id,
               rawOrLabel = tolower(raw_or_label), 
               rawOrLabelHeaders = tolower(raw_or_label_headers),
               exportCheckboxLabel = tolower(export_checkbox_label),
               csvDelimiter = csv_delimiter)
  
  body <- body[lengths(body) > 0]
  
  ##################################################################
  # Call the API
  
  response <- makeApiCall(rcon, 
                          body = body, 
                          ...)
  
  Report <- utils::read.csv(text = as.character(response), 
                            stringsAsFactors = FALSE, 
                            na.strings = "")
  
  Report
}
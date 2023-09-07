#' @describeIn recordsMethods Export data via a report.
#' @order 3
#' @export

exportReports <- function(rcon, 
                          report_id, 
                          raw_or_label          = c("raw", "label"),
                          raw_or_label_headers  = c("raw", "label"),
                          export_checkbox_label = FALSE,  
                          ...){
  UseMethod("exportReports")
}

#' @rdname recordsMethods
#' @order 5
#' @export

exportReports.redcapApiConnection <- function(rcon, 
                                              report_id, 
                                              raw_or_label          = c("raw", "label"),
                                              raw_or_label_headers  = c("raw", "label"),
                                              export_checkbox_label = FALSE,  
                                              ...,
                                              csv_delimiter         = c(",", "\t", ";", "|", "^"), 
                                              error_handling = getOption("redcap_error_handling"),
                                              config         = list(), 
                                              api_param      = list()){
  
  if (is.character(report_id)) report_id <- as.numeric(report_id)
  
  ###################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_integerish(x = report_id,
                               len = 1,
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
  # Make the Body List                                           ####
  
  body <- list(content = "report", 
               format = "csv", 
               returnFormat = "csv",
               report_id = report_id,
               rawOrLabel = raw_or_label, 
               rawOrLabelHeaders = raw_or_label_headers,
               exportCheckboxLabel = tolower(export_checkbox_label), 
               csvDelimiter = csv_delimiter)
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Make the API Call
  
  response <- makeApiCall(rcon, 
                          body = c(body, 
                                   api_param), 
                          config = config)
  
  if (response$status_code != 200){
    redcapError(response, 
                error_handling = error_handling)
  }
  
  utils::read.csv(text = as.character(response), 
                  stringsAsFactors = FALSE, 
                  na.strings = "")
}

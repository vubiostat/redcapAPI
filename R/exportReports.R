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
  
<<<<<<< HEAD
  if (response$status_code != 200){
    redcapError(response, 
                error_handling = error_handling)
=======
  if (response$status_code != 200) redcapError(response, error_handling)
  
  Report <- as.data.frame(response)
  
   ##################################################################
  # Process the data

  #* synchronize underscore codings between records and meta data
  #* Only affects calls in REDCap versions earlier than 5.5.21
  if (utils::compareVersion(version, "6.0.0") == -1) 
    MetaData <- syncUnderscoreCodings(Report, MetaData)
  

  Report <- fieldToVar(records = Report, 
                       meta_data = MetaData, 
                       factors = factors, 
                       dates = dates, 
                       labels=labels,
                       checkboxLabels = checkboxLabels,
                       ...)
  
  
  if (labels) 
  {
    field_names <- names(Report)
    field_names <- unique(sub(REGEX_CHECKBOX_FIELD_NAME, #defined in constants.R 
                              "\\1", field_names, perl = TRUE))
    
    # For reports, there is not check on the field names, since 
    # the user may only select fields using the interface.
    # However, [form]_complete fields do not appear in the 
    # meta data and need to be removed to avoid an error.
    # See #108
    field_names <- field_names[field_names %in% MetaData$field_name]

    suffixed <- checkbox_suffixes(fields = field_names,
                                  meta_data = MetaData)

    Report[suffixed$name_suffix] <-
      mapply(nm = suffixed$name_suffix,
             lab = suffixed$label_suffix,
             FUN = function(nm, lab){
               if(is.null(Report[[nm]])){
                 warning("Missing field for suffix ", nm)
               } else {
                 labelVector::set_label(Report[[nm]], lab)
               }
             },
             SIMPLIFY = FALSE)
>>>>>>> main
  }
  
  utils::read.csv(text = as.character(response), 
                  stringsAsFactors = FALSE, 
                  na.strings = "")
}

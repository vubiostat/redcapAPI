#' @describeIn recordsMethods Export data via a report.
#' @order 3
#' @export

exportReports <- function(rcon, 
                          report_id, 
                          factors        = TRUE, 
                          labels         = TRUE, 
                          dates          = TRUE, 
                          drop           = NULL,
                          checkboxLabels = FALSE, 
                          ...){
  UseMethod("exportReports")
}

#' @rdname recordsMethods
#' @order 5
#' @export

exportReports.redcapApiConnection <- function(rcon, 
                                              report_id, 
                                              factors        = TRUE, 
                                              labels         = TRUE, 
                                              dates          = TRUE, 
                                              drop           = NULL, 
                                              checkboxLabels = FALSE, 
                                              ...,
                                              error_handling = getOption("redcap_error_handling"),
                                              config         = list(), 
                                              api_param      = list()){
  
  if (!is.numeric(report_id)) report_id <- as.numeric(report_id)
  
   ##################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_integerish(x = report_id,
                               len = 1,
                               add = coll)
  
  checkmate::assert_logical(x = factors, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = labels, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = dates, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = checkboxLabels, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_character(x = drop, 
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
               report_id = report_id)
  
  body <- body[lengths(body) > 0]
  
   ##################################################################
  # Call the API
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) redcapError(response, error_handling)
  
  Report <- utils::read.csv(text = as.character(response), 
                            stringsAsFactors = FALSE, 
                            na.strings = "")
  
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
  }
  
   ##################################################################
  # Drop fields from Report
  
  if(length(drop)) {
    Report <- Report[!names(Report) %in% drop]
  } # end drop
  
  Report
}

#' @describeIn recordsMethods Export records from a project.
#' @order 1
#' @export

exportRecords <- function(rcon, 
                          ...){
  UseMethod("exportRecords")
}

#' @rdname recordsMethods
#' @order 4
#' @export

exportRecords.redcapApiConnection <- function(rcon, 
                                              records               = NULL, 
                                              fields                = NULL, 
                                              forms                 = NULL, 
                                              events                = NULL, 
                                              raw_or_label          = c("raw", "label"),
                                              raw_or_label_headers  = c("raw", "label"),
                                              export_checkbox_label = FALSE, 
                                              export_survey_fields  = FALSE, 
                                              export_dags           = FALSE,
                                              ..., 
                                              batch_size            = NULL, 
                                              csv_delimiter         = c(",", "\t", ";", "|", "^"), 
                                              error_handling        = getOption("redcap_error_handling"), 
                                              api_param             = list(), 
                                              config                = list()){
  
  if (is.numeric(records)) records <- as.character(records)
  
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = records, 
                              any.missing = FALSE, 
                              null.ok = TRUE, 
                              add = coll)
  
  checkmate::assert_character(x = fields, 
                              any.missing = FALSE, 
                              null.ok = TRUE, 
                              add = coll)
  
  checkmate::assert_character(x = forms, 
                              any.missing = FALSE, 
                              null.ok = TRUE, 
                              add = coll)
  
  checkmate::assert_character(x = events, 
                              any.missing = FALSE, 
                              null.ok = TRUE, 
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
  
  checkmate::assert_logical(x = export_survey_fields, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = export_dags, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_integerish(x = batch_size, 
                               len = 1, 
                               lower = 1,
                               any.missing = FALSE, 
                               null.ok = TRUE, 
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
  
  .exportRecordsTyped_validateFieldForm(rcon, 
                                        fields      = fields, 
                                        drop_fields = NULL, 
                                        forms       = forms, 
                                        coll        = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make the Body List                                           ####
  
  body <- list(content = "record", 
               format = "csv", 
               type = "flat", 
               returnFormat = "csv", 
               rawOrLabel = raw_or_label, 
               rawOrLabelHeaders = raw_or_label_headers,
               exportCheckboxLabel = tolower(export_checkbox_label), 
               exportSurveyFields = tolower(export_survey_fields), 
               exportDataAccessGroups = tolower(export_dags), 
               csvDelimiter = csv_delimiter)
  body <- c(body,  
            vectorToApiBodyList(fields, "fields"), 
            vectorToApiBodyList(forms, "forms"), 
            vectorToApiBodyList(events, "events"))
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Make the API Call                                            ####
  
  if (length(batch_size) > 0){
    .exportRecordsTyped_Batched(rcon, 
                                body          = body, 
                                records       = records, 
                                config        = config, 
                                api_param     = api_param, 
                                csv_delimiter = csv_delimiter, 
                                batch_size    = batch_size)
  } else {
    .exportRecordsTyped_Unbatched(rcon, 
                                  body          = body, 
                                  records       = records, 
                                  config        = config, 
                                  api_param     = api_param, 
                                  csv_delimiter = csv_delimiter)
  }
}

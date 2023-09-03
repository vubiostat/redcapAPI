#' @describeIn recordsMethods Import records to a project.
#' @order 4
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

#' @rdname recordsMethods
#' @order 8
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
  message("importRecords will change how it validates data in version 3.0.0.\n",
          "We recommend preparing your data for import using castForImport.")
  
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
  calc_field <- calc_field[calc_field %in% names(data)]
  
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

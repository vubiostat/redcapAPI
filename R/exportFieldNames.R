# Complete documentation in documentation.R
#' @rdname exportFieldNames 
#' @export


exportFieldNames <- function(rcon, 
                             ...){
  UseMethod("exportFieldNames")
}

#' @rdname exportFieldNames
#' @export

exportFieldNames.redcapApiConnection <- function(rcon, 
                                                 fields         = character(0), 
                                                 ...,
                                                 error_handling = getOption("redcap_error_handling"), 
                                                 config         = list(), 
                                                 api_param      = list()){
 
  # Argument validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_character(x = fields,
                              max.len = 1,
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
  
  if (length(fields) > 0){
    .exportFieldNames_validateFieldName(fields = fields, 
                                        rcon = rcon, 
                                        coll = coll)
  }

  # Build the Body List ---------------------------------------------
  body <- list(token = rcon$token, 
               content = 'exportFieldNames', 
               format = 'csv',
               returnFormat = 'csv', 
               field = fields)
  
  body <- body[lengths(body) > 0]
  
  # Make the API Call -----------------------------------------------
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200){
    redcapError(response, 
                 error_handling = error_handling)

  }
  
  utils::read.csv(text = as.character(response), 
                  stringsAsFactors = FALSE,
                  na.strings = "")
}

# Unexported --------------------------------------------------------

.exportFieldNames_validateFieldName <- function(fields, rcon, coll){
  # Get project metadata
  MetaData <- rcon$metadata()
  
  if (!all(fields %in% MetaData$field_name)){
    coll$push(sprintf("Field does not exist in the database: %s", 
                      fields))
    checkmate::reportAssertions(coll)
  }
}

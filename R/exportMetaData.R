# Complete documentation in documentation.R
#' @describeIn metaDataMethods Export the Meta Data (Data Dictionary) of a REDCap Project
#' @order 1
#' @export

exportMetaData <- function(rcon, ...){
  UseMethod("exportMetaData")
}

#' @rdname metaDataMethods
#' @export

exportMetaData.redcapApiConnection <- function(rcon, 
                                               fields = character(0), 
                                               forms = character(0),
                                               ...,
                                               error_handling = getOption("redcap_error_handling"), 
                                               config = list(), 
                                               api_param = list()){
  # Argument validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  checkmate::assert_character(x = fields, 
                              add = coll)
  
  checkmate::assert_character(x = forms, 
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
  
  if (!is.null(fields)){
    checkmate::assert_subset(x = fields, 
                             choices = rcon$metadata()$field_name, 
                             add = coll)
  }
  
  if (!is.null(forms)){
    checkmate::assert_subset(x = forms, 
                             choices = rcon$instruments()$instrument_name, 
                             add = coll)
  }
  
  checkmate::reportAssertions(coll)
  
  # Build the Body List ---------------------------------------------
  body <- c(list(token = rcon$token,
               content = "metadata",
               format = "csv",
               returnFormat = "csv"), 
            vectorToApiBodyList(fields, 
                                parameter_name = "fields"), 
            vectorToApiBodyList(forms, 
                                parameter_name = "forms"))
  
  body <- body[lengths(body) > 0]
 
  # API Call --------------------------------------------------------
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200){
    redcapError(response, 
                 error_handling = error_handling)
  }
  
  as.data.frame(response)
}

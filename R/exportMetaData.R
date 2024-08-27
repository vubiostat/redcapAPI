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
                                               ...)
{
  # Argument validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  checkmate::assert_character(x = fields, 
                              add = coll)
  
  checkmate::assert_character(x = forms, 
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
  body <- c(list(content = "metadata",
               format = "csv",
               returnFormat = "csv"), 
            vectorToApiBodyList(fields, 
                                parameter_name = "fields"), 
            vectorToApiBodyList(forms, 
                                parameter_name = "forms"))

  # API Call --------------------------------------------------------
  as.data.frame(makeApiCall(rcon, body, ...))
}

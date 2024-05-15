#' @name exportInstruments
#'
#' @title Export Instruments Defined in a Project
#' @description These methods enable the user to view the instruments defined
#'   in the project.
#'
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' 
#' @return 
#' Returns a data frame with the columns:
#' 
#' |                   |                                       |
#' |-------------------|---------------------------------------|
#' | `instrument_name`  | The REDCap generated instrument name. |
#' | `instrument_label` | The user provided instrument label.   |
#'
#'
#' @seealso
#' [exportMetaData()],\cr
#' [importMetaData()], \cr
#' [exportInstruments()],\cr
#' [exportMappings()],\cr
#' [importMappings()], \cr
#' [exportPdf()]
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'  
#' exportInstruments(rcon)
#' }
#'
#' @export

exportInstruments <- function(rcon, ...){
  UseMethod("exportInstruments")
}

#' @rdname exportInstruments
#' @export

exportInstruments.redcapApiConnection <- function(rcon, 
                                                  ...,
                                                  error_handling = getOption("redcap_error_handling"), 
                                                  config         = list(), 
                                                  api_param      = list()){
   ##################################################################
  # Argument Validation 
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
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
  # Make Body List
  
  body <- list(token = rcon$token, 
               content = 'instrument',
               format = 'csv')
  
  body <- body[lengths(body) > 0]
  
   ##################################################################
  # Call the API
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) return(redcapError(response, error_handling))
  
  as.data.frame(response)
}

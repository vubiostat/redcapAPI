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
                                                  ...)
{
   ##################################################################
  # Argument Validation 
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)

  checkmate::reportAssertions(coll)
  
   ##################################################################
  # Make Body List
  
  body <- list(content = 'instrument',
               format = 'csv')

   ##################################################################
  # Call the API
  as.data.frame(makeApiCall(rcon, body, ...))
}

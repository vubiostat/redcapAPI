#' @name mappingMethods
#' @title Export and Import Instrument-Event Mappings
#' 
#' @description These methods enable the user to export and add/modify the 
#'   mappings between instruments and events. The information provided 
#'   with the methods corresponds to what is provided in the 
#'   'Designate Instruments for My Events' page in the user interface.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param arms `integerish` or `character`. A vector of arm numbers. When 
#'   given, mappings are only exported for the given arms.
#' @param data `data.frame` with columns `arm_num`, `unique_event_name`, 
#'   and `form`. See Details
#'   
#' @details These methods are only applicable to longitudinal projects.
#'   If the project information reports that the project is not longitudinal, 
#'   a data frame with 0 rows is returned without calling the API. 
#'  
#' @return 
#' `exportMappings` returns a data frame with the columns:
#' 
#' |                     |                                                               |
#' |---------------------|---------------------------------------------------------------|
#' | `arm_num`           | The arm number for the unique event mapped to the instrument. | 
#' | `unique_event_name` | The unique event name to which the instrument is assigned.    | 
#' | `form`              | The REDCap assigned instrument name mapped to the event.      |
#' 
#' `importMappings` invisible returns the number of mappings added or edited.
#' 
#' @seealso 
#' [exportFieldNames()],\cr
#' [exportInstruments()],\cr
#' [exportMetaData()],\cr
#' [importMetaData()], \cr
#' [exportPdf()]
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#' 
#' # Export all mappings
#' exportMappings(rcon)
#' 
#' # Export mappings for a specific arm
#' exportMappings(rcon, 
#'                arms = 1)
#'                
#' # Import mappings
#' NewMapping <- 
#'   data.frame(arm_num = c(1, 1, 2), 
#'              unique_event_name = c("event_1_arm_1", 
#'                                    "event_2_arm_1", 
#'                                    "event_1_arm_2"), 
#'              form = c("registration", 
#'                       "follow_up", 
#'                       "registration"))
#' 
#' importMapping(rcon, 
#'               data = NewMapping)
#' }
#' 
#' @usage NULL
#' @order 0

mappingMethods <- function(rcon, 
                           arms, 
                           data, 
                           ...)
{
  NULL
}

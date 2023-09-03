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
#' @param arms \code{integerish} A vector of arm numbers that you wish to pull events for (by default,
#'   all events are pulled). Will also accept \code{character}, but will coerce
#'   it to numeric before processing.
#' @param data \code{data.frame} with columns \code{arm_num}, 
#'   \code{unique_event_name}, and \code{form}. See Details
#' @param refresh \code{logical(1)}. When \code{TRUE}, cached mappings 
#'   in the \code{rcon} object are refreshed after the import.
#'   
#' @details These methods are only applicable to longitudinal projects.
#'   If the project information reports that the project is not longitudinal, 
#'   a data frame with 0 rows is returned without calling the API. 
#'  
#' @return 
#' \code{exportMappings} returns a data frame with the columns:
#' \tabular{ll}{
#'  \code{arm_num} \tab The arm number for the unique event mapped to the 
#'    instrument. \cr
#'  \code{unique_event_name} \tab The unique event name to which the 
#'    instrument is assigned. \cr
#'  \code{form} \tab The REDCap assigned instrument name mapped to the event.
#' } 
#' 
#' 
#' @seealso 
#' \code{\link{exportFieldNames}},\cr
#' \code{\link{exportInstruments}},\cr
#' \code{\link{exportMetaData}},\cr
#' \code{\link{importMetaData}}, \cr
#' \code{\link{exportPDF}}
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
                           refresh, 
                           ..., 
                           error_handling, 
                           config, 
                           api_param){
  NULL
}

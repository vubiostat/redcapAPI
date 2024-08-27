#' @name surveyMethods
#' @title Export Survey Participant Information
#'
#' @description These methods enable the user to export information relating
#'   to survey participants. 
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param record `character(1)` or `integerish(1)`. The record ID of a 
#'   survey participant.
#' @param instrument `character(1)`. The name of a survey instrument.
#' @param event `character(1)` The event name of the event for which 
#'   participant information should be exported. 
#' @param repeat_instance `integerish(1)`. The repeat instance if the 
#'   instrument is designated as a repeating instrument. Default value is `1`.
#'   
#' @return 
#' 
#' `exportSurveyParticipants` returns a data frame with the columns:
#' 
#' |                      |                                        |
#' |----------------------|----------------------------------------|
#' | `email`              | The e-mail address of the participant. | 
#' | `email_occurrence`   | The number of times the invitation has been sent (after the next invite). | 
#' | `identifier`         | Participant identifier (if it exists) to match the survey response to a participant. |
#' | `record`             | Record ID of the participant. |
#' | `invitation_sent_status` | Boolean value indicating if a survey invitation has been sent. |
#' | `invitation_send_time`   | Date/time the survey invitation was sent. |
#' | `response_status`    | Boolean value indicating if the participant has responded. |
#' | `survey_access_code` | The participant's survey access code. |
#' | `survey_link`        | The participant's survey link. | 
#' | `survey_queue_link`  | The participants' survey queue link. |
#' 
#' `exportSurveyLink` returns a `character(1)` giving the link for the user
#' to access the survey form. 
#' 
#' `exportSurveyQueueLink` returns a `character(1)` giving the survey queue
#' link for the user. 
#' 
#' `exportSurveyReturnCode` returns a `character(1)` giving the survey return
#' code for the user. 
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' # Export survey participants
#' exportSurveyParticipants(rcon, 
#'                          instrument = "survey_form")
#'                          
#' # Export survey participants for an event
#' exportSurveyParticipants(rcon, 
#'                          instrument = "survey_form", 
#'                          event = "event_1_arm_1")
#'                          
#' # Export survey link
#' exportSurveyLink(rcon, 
#'                  record = 1, 
#'                  instrument = "survey_form")
#'                  
#' # Export survey queue link
#' exportSurveyQueueLink(rcon, 
#'                       record = 1)
#'                       
#' # Export survey return code
#' exportSurveyReturnCode(rcon, 
#'                        user = 1, 
#'                        instrument = "survey_form")
#' }
#'
#' @usage NULL
#' @order 0

surveyMethods <- function(rcon,
                          record,
                          instrument, 
                          event, 
                          repeat_instance,
                          ...)
{
  NULL
}

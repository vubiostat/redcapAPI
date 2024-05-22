#' @name repeatingInstrumentMethods
#' @title Export or Import Repeating Instrument and Events Settings
#' 
#' @description These methods enable the user to export the existing 
#'   repeating instrument and event settings, or import new settings to
#'   the project. 
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param data `data.frame`. For classical projects, it must have the
#'   columns `form_name` and `custom_form_label`. Longitudinal
#'   projects also require a column for `event_name`.
#'   
#' @details Repeating events (as opposed to repeating instruments) are 
#'   provided as a row of data where the `form_name` column is `NA`.
#'   
#'   It is not possible to update the `has_repeating_instruments_or_events` 
#'   property of the project through `importProjectInformation`. 
#'   Enabling of repeating instruments and events must be done through the GUI.
#' 
#'   Although the API does not provide a delete method, it is possible to 
#'   remove settings by doing an import that excludes the settings that are
#'   to be deleted. All settings can be cleared by executing
#'   `importRepeatingInstrumentsEvents(rcon, REDCAP_REPEAT_INSTRUMENT_STRUCTURE)`.
#'   
#' @return
#' `exportRepeatingInstrumentsEvents` returns a data frame with the columns:
#' |                     |                                                               |
#' |---------------------|---------------------------------------------------------------|
#' | `event_name`        | The unique event name.                                        | 
#' | `form_name`         | The form name, as given in the second column of the Meta Data | 
#' | `custom_form_label` | A custom display string for the repeating instrument/event    |
#' 
#' `importRepeatingInstrumentsEvents` invisibly returns the number of rows imported.
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' # Export repeating instruments and events
#' exportRepeatingInstrumentsEvents(rcon)
#' 
#' # Import repeating instruments and events
#' NewData <- data.frame(event_name = c("event_1_arm_1", 
#'                                      "event_2_arm_1"), 
#'                       form_name = c("field_observation", 
#'                                     "self_assessment"), 
#'                       custom_form_label = c("Instructor led field observation", 
#'                                             "Trainee self assessment"))
#'                                             
#' importRepeatingInstrumentsEvents(rcon, 
#'                                  data = NewData)
#' }
#' 
#' @usage NULL
#' @order 0

repeatingInstrumentEventMethods <- function(rcon, 
                                            data,
                                            ...)
{
  NULL
}

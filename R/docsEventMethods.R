# Events Methods ####################################################
#' @name eventsMethods
#' @title Export, Import, and Delete Event Settings
#' 
#' @description These methods enable the user to export event settings, 
#'   import new events, update settings for existing events, or 
#'   delete events. 
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param arms `character` or `integerish` identifying the arm 
#'   numbers for which event data will be exported.
#' @param events `character` giving the unique event names of the events to 
#'   be deleted.
#' @param data `data.frame`. Must have columns `event_name` and `arm_num`. 
#'   To modify existing events, it must also have a column `unique_event_name`. 
#'   It may optionally have columns for `days_offset`, `offset_min`, `offset_max`. 
#'   For backward compatibility, this argument may be passed as `event_data`.
#' @param override `logical(1)`. By default, data will add to or modify 
#'   existing arms data. When `TRUE`, all the existing arms data is 
#'   deleted and replaced with the contents of `data`.
#' 
#' @details
#' Exporting events is not supported for classical REDCap projects. If 
#'   the user attempts to export arms for a classical project, a 
#'   data frame will be returned with zero rows.
#'   
#' Additionally, in order for events to be exported, the project must be
#'   longitudinal, have at least one arm, and at least one event defined.
#'   When these conditions are not satifisfied, `exportEvents` 
#'   will return a data frame with zero rows.
#'   
#' To import new events, the user must provide data with the 
#'   `unique_event_name` set to `NA` (REDCap assigns the unique
#'   event name automatically from the user provided `event_name`). 
#'   
#' To modify existing events, the user must provide the `unique_event_name`. 
#'   The other fields in the data provided will overwrite the current values
#'   for the matching event. 
#'   
#' Deleting events--whether by `deleteEvents` or `importEvents` with 
#'   `override = TRUE`--is a destructive act that also deletes 
#'   arms and records associated with the event. This is irreversible 
#'   data loss. REDCap will only permit these actions to occur in projects
#'   in Development status.
#' 
#' @return
#' `exportEvents` returns a data frame with the columns:
#' 
#'  |                    |                                       |
#'  |--------------------|---------------------------------------|
#'  | event_name         | The user provided name for the event. |
#'  | arm_num            | The arm number the event is associated with. |
#'  | unique_event_name  | The REDCap generated event name. |
#'  | custom_event_label | An optional user provided label that may be used in place of the event name. |
#'  | event_id           | REDCap's internal event identifier. |
#'  | days_offset        | The number of days since time zero (start of the study or project period) an event is scheduled to occur. This field is only provided when the scheduling module is enabled. |
#'  | offset_min         | The number of days before the `days_offset` during which the event may occur. This field is only provided when the scheduling module is enabled. |
#'  | offset_max         | The number of days before the `days_offset` during which the event may occur. This field is only provided when  the scheduling module is enabled. |
#' 
#' `importEvents` invisibly returns the number of events added or modified.
#'   
#' `deleteEvents` invisibly returns the number of events deleted.
#'   
#' @seealso 
#' [exportMappings()], \cr
#' [importMappings()]
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'
#' # Export all events
#' exportEvents(rcon)
#' 
#' # Export events for a subset of arms
#' exportEvents(rcon, 
#'              arms = c(1, 3))
#'              
#' # Import new events
#' NewEvents <- data.frame(event_name = c("Event 1", 
#'                                        "Event 2"), 
#'                         arm_num = c(1, 1))
#' importEvents(rcon, 
#'              data = NewEvents)
#'              
#' # Modify existing events
#' UpdateEvents <- data.frame(event_name = "Event 2 New Name", 
#'                            arm_num = 1, 
#'                            unique_event_name = "event_2_arm_1", 
#'                            custom_event_label = "The second visit")
#' importEvents(rcon, 
#'              data = UpdateEvents)
#'              
#' # Replace all events with a new set
#' NewEvents <- data.frame(event_name = c("Event 1", 
#'                                        "Event 2", 
#'                                        "Event 1"), 
#'                         arm_num = c(1, 1, 2))
#' importEvents(rcon, 
#'              data = NewEvents, 
#'              override = TRUE)
#'              
#' # Delete events
#' deleteEvents(rcon, 
#'              events = c("event_1_arm_1", "event_1_arm_2"))
#' }
#' 
#' @usage NULL
#' @order 0
# dummy function to control the order of arguments in the help file.
eventsMethods <- function(rcon, 
                          arms, 
                          events, 
                          data, 
                          override, 
                          ...)
{
  NULL
}

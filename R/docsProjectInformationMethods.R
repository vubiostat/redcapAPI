#' @name projectInformationMethods
#' @title Export and Import Project Settings
#' 
#' @description These methods enable the user to export or update project
#'   level settings, such as the project title, if it is longitudinal, 
#'   if surveys are enabled, etc.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param data `data.frame` with only one row and any subset of allowable fields to be
#'   updated. See Details.
#'   
#' @details
#' When importing, fields that are not editable will be quietly removed prior to 
#'   import. This allows the user to use the result of 
#'   `exportProjectInformation` as a template for the import.
#'   
#'   For any values that are boolean, they should be represented as either a '0' 
#'   (no/false) or '1' (yes/true).
#'   
#'   It is not required for `data` to have all of the fields, but only
#'   the fields the user wishes to update (see examples). 
#'   
#'   The following project attributes can be updated:
#' 
#' * project_title
#' * project_language
#' * purpose
#' * purpose_other
#' * project_notes
#' * custom_record_label
#' * secondary_unique_field
#' * is_longitudinal
#' * surveys_enabled
#' * scheduling_enabled
#' * record_autonumbering_enabled
#' * randomization_enabled
#' * project_irb_number
#' * project_grant_number
#' * project_pi_firstname
#' * project_pi_lastname
#' * display_today_now_button
#' * bypass_branching_erase_field_prompt
#' 
#' @return
#' `exportProjectInformation` returns a data frame with the columns
#' 
#' |              |                                                 |
#' |--------------|-------------------------------------------------|
#' | `project_id` | The internal ID number assigned to the project. |
#' | `project_title` | The project title given to the project. |
#' | `creation_time` | The date/time the project was created. |
#' | `production_time` | The date/time the project was moved into production status. |
#' | `in_production` | Boolean value indicating if the project is in production status. |
#' | `project_language` | The language associated with the project. |
#' | `purpose` | An integerish value identifying the purpose of the project. 0 = "Practice/Just for fun", 1 = "Other", 2 = "Research", 3 = "Quality Improvement", 4 = "Operational Support". |
#' | `purpose_other` | The user supplied character value given when the project purpose is 'Other'. |
#' | `project_notes` | The user supplied notes about the project. |
#' | `custom_record_label` | The user provided custom label for the record identifier field. |
#' | `secondary_unique_field` | The name of the secondary unique field, if this has been configured. | 
#' | `is_longitudinal` | Boolean value indicating if the project is a longitudinal project. |
#' | `has_repeating_instruments_or_events` | Boolean value indicating if the repeating instruments or events module has been enabled. |
#' | `surveys_enabled` | Boolean value indicating if the surveys module has been enabled. |
#' | `scheduling_enabled` | Boolean value indicating if the scheduling module has been enabled. |
#' | `record_autonumbering_enabled` | Boolean value indicating if the record autonumbering feature has been enabled. |
#' | `randomization_enabled` | Boolean value indicating if the randomization module has been enabled. |
#' | `ddp_enabled` | Boolean value indicating if dynamic data pull has been enabled for a project (may only be enabled by a REDCap administrator). |
#' | `project_irb_number` | The user provided IRB number for the project. |
#' | `project_grant_number` | The user provided grant number for the project. |
#' | `project_pi_firstname` | The first name of the principal investigator. |
#' | `project_pi_lastname` | The last name of the principal investigator. |
#' | `display_today_now_button` | Boolean value indicating if the today/now button is displayed for date/time fields in the UI. |
#' | `missing_data_codes` | Character value giving the missing data codes enabled for the project. They are given in the format `[code],[label]`, with each coding separated by a pipe character. |
#' | `external_modules` | Character value listing the external modules enabled. |
#' | `bypass_branching_erase_field_prompt` | Boolean value indicating if the box for "Prevent branching logic from hiding fields that have values" has been checked under "Additional Customizations."
#'
#' 
#' `importProjectInformation` invisibly returns the number of fields updated.
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#' 
#' # Export Project Information
#' exportProjectInformation(rcon)
#' 
#' # Import a new project title
#' NewData <- data.frame(project_title = "New Title Name")
#' importProjectInformation(rcon, 
#'                          data = NewData)
#'                          
#' # Enable surveys in the project
#' NewData <- data.frame(surveys_enabled = 1)
#' importProjectInformation(rcon, 
#'                          data = NewData)
#'                          
#' # Change multiple fields in the project settings
#' NewData <- data.frame(project_irb_number = "IRB-12345", 
#'                       display_today_now_button = 0, 
#'                       scheduling_enabled = 1)
#' importProjectInformation(rcon, 
#'                          data = NewData)
#' }
#' 
#' @usage NULL
#' @order 0

projectInformationMethods <- function(rcon, 
                                      data,
                                      ...)
{
  NULL
}

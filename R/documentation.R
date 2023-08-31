# Arms Methods ######################################################
#' @name armsMethods
#' @aliases deleteArms, exportArms, importArms
#' 
#' @title Export, Import, and Delete Arms from a Project
#' 
#' @description These methods enable the user to export the current arms
#'   from a project, import new arms, and modify or delete existing arms.
#'   
#' @inheritParams common-rcon-arg
#' @param data A \code{data.frame} with two columns.  The first column 
#'   (\code{arm_num}) is an integerish value . The second (\code{name}) is
#'   a character value. For backward compatibility, 
#'   this may also be passed as \code{arms_data}.
#' @param override \code{logical(1)}. By default, data will add to or modify 
#'   existing arms data. When \code{TRUE}, all the existing arms data is 
#'   deleted and replaced with the contents of \code{data}.
#' @param refresh \code{logical(1)} If \code{TRUE}, the cached arms data will
#'   be refreshed after the API action is complete.
#' @param arms \code{character} or \code{integerish} identifying the arm 
#'   numbers to export or delete.
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' 
#' @details 
#'   Exporting arms is not supported for classical REDCap projects. If 
#'   the user attempts to export arms for a classical project, a 
#'   data frame will be returned with zero rows.
#' 
#'   When importing, arms are added when the value of \code{arm_num}
#'   does not already exist in the project. 
#'   
#'   Arm names may be modified by altering the \code{name} value associated
#'   with an existing \code{arm_num} value. 
#'   
#'   Deleting arms--whether by \code{deleteArms} or \code{importArms} with 
#'   \code{override = TRUE}--is a destructive act that also deletes 
#'   events and records associated with the arm. This is irreversible 
#'   data loss. REDCap will only permit these actions to occur in projects
#'   in Development status.
#'   
#' @return
#' \code{exportArms} returns a \code{data.frame} with columns:
#' \itemize{
#'  \item{\code{arm_num} }{The ID number for the arm in the project.}
#'  \item{\code{name} }{The display name of the arm.}
#' }
#' 
#' \code{importArms} has no return and prints a message indicating the 
#'   number of arms imported.
#'   
#' \code{deleteArms} has no return and prints a message indicating the
#'   number of arms deleted.
#'   
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'
#' # Export all of the Arms
#' exportArms(rcon)
#' 
#' # Export only a subset of arms
#' exportArms(rcon, 
#'            arms = c(1, 3))
#' 
#' 
#' # Import a new arms
#' # Assume arms 1, 2, and 3 exist in the project already
#' NewData <- data.frame(arm_num = 4, 
#'                       name = "Arm Four Name")
#' importArms(rcon, 
#'            data = NewData)
#'            
#' # Change the name of an existing arm
#' NewData <- data.frame(arm_num = 1, 
#'                       name = "New Arm Name")
#' importArms(rcon, 
#'            data = NewData)
#'            
#' # Delete all arms and replace with a new specification
#' NewData <- data.frame(arm_num = c(1, 2), 
#'                       name = c("Treatment Arm", "Control Arm"))
#' importArms(rcon, 
#'            data = NewData, 
#'            override = TRUE)
#'            
#' # Delete an existing arm
#' deleteArms(rcon, 
#'            arms = 4)
#'          
#' # Delete multiple existing arm
#' deleteArms(rcon, 
#'            arms = c(2, 3))
#' }

NULL

# DAG Methods #######################################################
#' @name dagMethods
#' @aliases deleteDags, exportDags, importDags
#' 
#' @title Export, Import, Delete Data Access Groups from a Project
#' 
#' @description These methods enables the user to export existing Data Access Groups, 
#'   import new Data Access Groups, or delete Data Access Groups from a 
#'   project.
#'   
#' @inheritParams common-rcon-arg
#' @param data A \code{data.frame} with two columns: \code{data_access_group_name}
#'   and \code{unique_group_name}. 
#' @param dags \code{character} vector of names matching the \code{unique_group_name}.
#' @param refresh \code{logical(1)}. When \code{TRUE}, cached data access
#'   group data will be refreshed after the import.
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#'   
#' @details To import new data access groups, the user must provide a value for 
#'   \code{data_access_group_name} with no value (\code{NA}) for \code{unique_group_name}. 
#'   
#'   To modify a group name, provide a new value for \code{data_access_group_name}
#'   with the associated \code{unique_group_name}. If \code{unique_group_name}
#'   is provided, it must match a value currently in the project.
#'
#' @seealso
#' \code{\link{switchDag}},\cr
#' \code{\link{exportUserDagAssignments}}, \cr
#' \code{\link{importUserDagAssignments}}
#' 
#' @return 
#' \code{exportDags} with the columns
#' \itemize{
#'   \item{\code{data_access_group_name}}{The human readable name for the data access group.}
#'   \item{\code{unique_group_name}}{The internal unique group name}
#'   \item{\code{data_access_group_id}}{The internal numeric identifier.}
#' }
#' 
#' \code{importDags} has no return, but will print a message indicating the
#' number of Data Access Groups imported. 
#' 
#' \code{deleteDags} has no return, but will print a message indicating the
#' number of Data Access Groups deleted.
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'
#' exportDags(rcon)
#' 
#' 
#' # Import a new Data Access Group
#' NewData <- data.frame(data_access_group_name = "New DAG Name", 
#'                       unique_group_name = NA_character_)
#' importDags(rcon, 
#'            data = NewData)
#'            
#' # Modify an existing Data Access Group Name
#' # The user will need to match the unique_group_name to the existing DAGs
#' ChangeData <- data.frame(data_access_group_name = "Altered DAG Name", 
#'                          unique_group_name = "new_dag_name")
#' importDags(rcon, 
#'            data = ChangeData)
#'            
#' # Delete a Data Access Group
#' deleteDags(rcon, 
#'            dags = c("new_dag_name"))
#' }
#' 

NULL

# switchDag #########################################################

#' @name switchDag
#' @title Switch Data Access Group Assignment for the Current User
#' 
#' @description This method enables the current API user to switch 
#'   (assign/reassign/unassign) their current Data Access Group assignment 
#'   if they have been assigned to multiple DAGs via the DAG Switcher page 
#'   in the project. 
#'   
#' @inheritParams common-rcon-arg
#' @param dag \code{character(1)} A unique data access group to which to 
#'   assign the current user. Use \code{NA} to leave the user unassigned.
#' @param refresh \code{logical(1)} If \code{TRUE}, the cached data access
#'   group assignments will be refreshed.
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#'   
#' @return Returns \code{TRUE} when the call is completed successfully.
#' 
#' @seealso 
#' \code{\link{exportDags}},\cr
#' \code{\link{importDags}},\cr
#' \code{\link{deleteDags}}, \cr
#' \code{\link{exportUserDagAssignments}}, \cr
#' \code{\link{importUserDagAssignments}}
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'
#' # Switch the current user to the DAG "Facility Two"
#' switchDag(rcon, 
#'           dag = "facility_two")
#' }

NULL

# DAG Assignment Methods ############################################
#' @name dagAssignmentMethods
#' @aliases exportUserDagAssignments, importUserDagAssignments
#' @title Export and Import Users Assigned to Data Access Groups
#' 
#' @description These methods enable the user to export existing assignments 
#'   of users to Data Access Groups, or import new or updated assignments 
#'   to the project. 
#'   
#' @inheritParams common-rcon-arg
#' @param data \code{data.frame} with the columns \code{username} and 
#'   \code{redcap_data_access_group}. The should only be one row per 
#'   user name.
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' 
#' @details When modifying existing assignments using the import method, 
#'   the user must provide the
#'   unique user name and the group name. If the \code{redcap_data_access_group}
#'   column is not provided, the REDCap user will not be assigned to any group. 
#' 
#' @return 
#' \code{exportUserDagAssignments} method returns a data frame with two columns:
#' \itemize{
#'  \item{username}{The unique user name for each user in the project.}
#'  \item{redcap_data_access_grou}{The unique Data Access Group name to which the user is assigned.}
#' }
#' 
#' \code{importUserDagAssignments} has no return and prints a message indicating the number 
#'   of assignments imported.
#'   
#' @seealso 
#' \code{\link{exportDags}},\cr
#' \code{\link{importDags}},\cr
#' \code{\link{deleteDags}},\cr
#' \code{\link{switchDag}}
#' 
#'@examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'
#' # Export the current assignments
#' exportUserDagAssignments(rcon)
#' 
#' # Assign a user to a Data Access Group
#' ForImport <- data.frame(username = "user1", 
#'                         redcap_data_access_group = "facility_one")
#' importUserDagAssigments(rcon, 
#'                         data = ForImport)
#'                                           
#' # Assign a multiple users to a Data Access Group
#' ForImport <- data.frame(username = c("user1", "user2", "user3"), 
#'                         redcap_data_access_group = c("facility_one", 
#'                                                      "facility_one", 
#'                                                       "facility_two"))
#' importUserDagAssigments(rcon, 
#'                         data = ForImport)
#'                         
#' # Remove a user from all Data Access Groups
#' ForImport <- data.frame(username = "user1", 
#'                         redcap_data_access_group = NA_character_)
#' importUserDagAssigments(rcon, 
#'                         data = ForImport)
#' }

NULL

# Events Methods ####################################################
#' @name eventsMethods
#' @aliases deleteEvents, exportEvents, importEvents
#' @title Export, Import, and Delete Event Settings
#' 
#' @description These methods enable the user to export event settings, 
#'   import new events, update settings for existing events, or 
#'   delete events. 
#'   
#' @inheritParams common-rcon-arg
#' @param arms \code{character} or \code{integerish} identifying the arm 
#'   numbers for which event data will be exported.
#' @param events \code{character} giving the unique event names
#'   of the events to be deleted.
#' @param data \code{data.frame}. Must have columns \code{event_name}
#'   and \code{arm_num}. To modify existing events, it must also have a column
#'   \code{unique_event_name}. It may optionally have columns for 
#'   \code{days_offset}, \code{offset_min}, \code{offset_max}. 
#'   For backward compatibility, this argument may be passed as \code{event_data}.
#' @param override \code{logical(1)}. By default, data will add to or modify 
#'   existing arms data. When \code{TRUE}, all the existing arms data is 
#'   deleted and replaced with the contents of \code{data}.
#' @param refresh \code{logical(1)} If \code{TRUE}, the cached arms data will
#'   be refreshed after the API action is complete.
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' 
#' @details
#' Exporting events is not supported for classical REDCap projects. If 
#'   the user attempts to export arms for a classical project, a 
#'   data frame will be returned with zero rows.
#'   
#' Additionally, in order for events to be exported, the project must be
#'   longitudinal, have at least one arm, and at least one event defined.
#'   When these conditions are not satifisfied, \code{exportEvents} 
#'   will return a data frame with zero rows.
#'   
#' To import new events, the user must provide data with the 
#'   \code{unique_event_name} set to \code{NA} (REDCap assigns the unique
#'   event name automatically from the user provided \code{event_name}). 
#'   
#' To modify existing events, the user must provide the \code{unique_event_name}. 
#'   The other fields in the data provided will overwrite the current values
#'   for the matching event. 
#'   
#' Deleting events--whether by \code{deleteEvents} or \code{importEvents} with 
#'   \code{override = TRUE}--is a destructive act that also deletes 
#'   arms and records associated with the event. This is irreversible 
#'   data loss. REDCap will only permit these actions to occur in projects
#'   in Development status.
#' 
#' @returns 
#' \code{exportEvents} returns a data frame with the columns:
#' \itemize{
#'  \item{\code{event_name }}{The user provided name for the event.}
#'  \item{\code{arm_num }}{The arm number the event is associated with.}
#'  \item{\code{unique_event_name }}{The REDCap generated event name.}
#'  \item{\code{custom_event_label }}{An optional user provided label that 
#'    may be used in place of the event name.}
#'  \item{\code{event_id }}{REDCap's internal event identifier.}
#'  \item{\code{days_offset }}{The number of days since time zero (start of 
#'    the study or project period) an event is scheduled to occur. This
#'    field is only provided when the scheduling module is enabled.}
#'  \item{\code{offset_min }}{The number of days before the \code{days_offset}
#'    during which the event may occur. This field is only provied when 
#'    the scheduling module is enabled.}
#'  \item{\code{offset_max }}{The number of days before the \code{days_offset}
#'    during which the event may occur. This field is only provied when 
#'    the scheduling module is enabled.}
#' }
#' 
#' \code{importEvents} has no return and prints a message indicating how many
#'   events were added or modified.
#'   
#' \code{deleteEvents} has no return and prints a message indicating how many
#'   events were deleted.
#'   
#' @seealso 
#' \code{\link{exportMappings}}, \cr
#' \code{\link{importMappings}}
#' 
#' @examples
#' #' \dontrun{
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

NULL

# Meta Data Methods (Data Dictionary) ###############################
#' @name metaDataMethods
#' @aliases exportMetaData, importMetaData
#' @title Export and Import the Project Meta Data (Data Dictionary)
#' 
#' @description These methods provide the user access to a REDCap project's
#'   data dictionary. The data dictionary may be exported or altered via
#'   the import.
#'   
#'   The user may also access the field names in the project, including 
#'   the full names for checkbox values.
#'
#' @inheritParams common-rcon-arg
#' @param fields \code{character} vector of field names for which the metadata is to 
#'   be retrieved. 
#' @param forms \code{character} vector of forms for which the metadata is to be
#'   retrieved. If a form name is given, all of the fields on that form
#'   will be returned, regardless of whether it is included in \code{fields} or 
#'   not.  Form names should match those in the second column of the data 
#'   dictionary, and not the display names shown on the web interface.
#' @param data \code{data.frame} with the Meta Data to import. 
#' @param refresh \code{logical(1)}. When \code{TRUE}, the cached metadata
#'   and instruments will be refreshed after the import.
#' @param field_types \code{character} giving the acceptable field types
#'   when validating the \code{field_type} column. This  
#' @param validation_types \code{character} giving the acceptable values 
#'   for the \code{text_validation_or_show_slider_number} column.
#' @param drop_utf8 \code{logical(1)}. When \code{TRUE}, non-ASCII characters 
#'   will be replaced with empty characters.
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#'   
#' @details
#' When importing meta data, the following conditions apply:
#' 
#' Field names may start with a letter, have any number of letters, 
#'   numbers, or underscores, and end in either a letter or a number. All 
#'   letters must be lowercase (the function will coerce them to lower before
#'   checking for duplicate field names). 
#'   
#' Form names may start with a letter, have any number of letters, 
#'   numbers, or underscores, and end in either a letter or a number. All 
#'   letters must be lowercase (the function will coerce them to lower before
#'   checking for duplicate field names).
#'   
#' Field types may be one of \code{REDCAP_METADATA_FIELDTYPE}. In the event that a 
#'   new field type is added to REDCap and \code{redcapAPI} isn't yet updated, 
#'   you may add additional values via \code{c(REDCAP_METADATA_FIELDTYPE, "new_type")}.
#'   
#' Validation types may be one of \code{REDCAP_METADATA_VALIDATION_TYPE} or 
#'  \code{NA}. AS with field types, additional values can be appended if
#'  necessary. Only fields that have a field type of "text" or "slider" 
#'  should have a validation type. "slider" fields should be either \code{NA}
#'  (do not display the selected number) or \code{"number"}.
#'  
#' For multiple choice fields, the selection choices take the format of 
#'   \code{"code1, label1 | ... | coden, labeln"}. For slider fields, the 
#'   format is \code{"left_value | mid_value | right_value"}. Any of those 
#'   values may be an empty character, but the two pipes are required, nonetheless.
#' 
#' For calculated fields, the values in \code{"select_choices_or_calculations"}
#'   are currently unvalidated.  
#' 
#' All of the values between brackets in the branching logic must be either a
#'   field name or an existing unique event name (such as \code{"event_1_arm_1"})
#'   
#' @return
#' 
#' \code{exportMetaData} returns a data frame. Not all 18 (or more) columns are
#' documented here, but the most commonly used within \code{redcapAPI} are 
#' (these may appear in a different order in the data frame):
#' \itemize{
#'  \item{\code{field_name} }{The name of a field in the project.}
#'  \item{\code{filed_label} }{The human-readable form of the field name.}
#'  \item{\code{form_name} }{The name of the form on which the field is found.}
#'  \item{\code{field_type} }{One of two fields used to determine how a field
#'        is transformed into an R object.}
#'  \item{\code{select_choices_or_calculations} }{The second field used to 
#'        determine how a field is translated into an R object.}
#'  \item{\code{text_validation_type_or_show_slider_number} }{Describes how 
#'        fields are validated. For slider fields, it gives the limits and
#'        center point to display.}
#'  \item{\code{field_annotation} }{Contains annotations such as units of
#'       measures. Also contains action tags.}    
#' }
#' 
#' \code{importMetaData} has no return and displays a message indicating 
#'   the number of fields that were imported.
#' 
#' @seealso 
#' \code{\link{exportFieldNames}},\cr
#' \code{\link{exportInstruments}},\cr
#' \code{\link{exportMappings}},\cr
#' \code{\link{importMappings}}
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'
#' # Export the MetaData from REDCap
#' exportMetaData(rcon)
#' 
#' # Export MetaData for select fields only (returns two rows)
#' exportMetaData(rcon, 
#'                fields = c("dropdown_test", "radio_test"))
#' 
#' # Export MetaData for select forms
#' exportMetaData(rcon, 
#'                forms = c("first_form", "second_form"))
#'                
#' # MetaData my be exported for a combination of fields and forms
#' exportMetaData(rcon, 
#'                fields = c("dropdown_test", "radio_test"), 
#'                forms = c("first_form", "second_form"))
#'                
#' # Alter and import new MetaData (change the record ID label)
#' Meta <- exportMetaData(rcon)
#' 
#' Meta$field_label[1] <- "A better description of the Record ID"
#' importMetaData(rcon, 
#'                data = Meta)
#' }

NULL

# Export Field Names ################################################
#' @name exportFieldNames
#' @aliases exportFieldNames
#' @title Export the Complete Field Names for a REDCap Project
#' 
#' @description This method enables the user to access the complete field 
#'   names utilized during export and import methods. These are expecially
#'   relevant when working with checkbox fields.
#'   
#' @inheritParams common-rcon-arg
#' @param fields \code{NULL} or \code{character(1)}. Field name to be returned.  By 
#'   default, all fields are returned.
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' 
#' @details
#' \code{exportFieldNames} returns a data frame of the field names the user
#' may use when performing export and import functions. This is most useful 
#' when working with checkbox fields, which have a different field name than 
#' the one used in the Meta Data. The exported/imported field names for 
#' checkbox fields have the pattern \code{[field_name]___[coded_checkbox_value]}
#' (there are exactly three underscores separating the field name and the
#' coded value).
#' 
#' Fields of types "calc", "file", and "descriptive" are not included in the
#' export. (Signature fields also have the "file" type and are not included)
#' 
#' @return 
#' #' \code{exportFieldNames} returns a data frame with the columns: 
#' \itemize{
#'   \item{\code{original_field_name} }{The field name as recorded in the 
#'        data dictionary}
#'   \item{\code{choice_value} }{represents the raw coded value for a checkbox 
#'        choice. For non-checkbox fields, this will always be \code{NA}.}
#'   \item{\code{export_field_name} }{The field name specific to the field.
#'        For non-checkbox fields, this is the same as \code{original_field_name}.
#'        For checkbox fields, it is the field name appended with 
#'        \code{___[choice_value]}.}
#' }
#' 
#' @seealso 
#' \code{\link{exportMetaData}},\cr
#' \code{\link{importMetaData}}, \cr
#' \code{\link{exportInstruments}},\cr
#' \code{\link{exportMappings}},\cr
#' \code{\link{importMappings}}
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'
#' # Export all of the field names
#' exportFieldNames(rcon)
#' 
#' # Export MetaData for a specific field
#' exportFieldNames(rcon, 
#'                  fields = "checkbox_test")
#' }

NULL
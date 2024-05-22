#' @name userMethods
#' @title Export, Import, or Delete Users and User Permissisons
#' 
#' @description These methods enable the user to add and remove users from
#'   a project. They also enable the user to modify the permissions 
#'   granted to each user within the project.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param dates `logical(1)`. When `TRUE`, expiration dates are converted to a
#'   `POSIXct` object.
#' @param labels `logical(1)`. When `TRUE` the data export and form access rights are
#'   converted to factor objects.
#' @param form_rights `logical(1)`. When `TRUE`, the form rights will be 
#'   transformed to one column per form. The API-provided character string
#'   is always returned with the format `[form_name]:[access_code]` and a comma separating
#'   each form.
#' @param users `character`. Vector of unique user names to be deleted.
#' @param data `data.frame`. Provides the user data for import. It must have
#'   a column titled `username`. All other columns are optional.
#' @param consolidate `logical(1)`. When `TRUE`, the form and data 
#'   export access values will be read from the expanded columns. Otherwise, 
#'   the consolidated values (as provided by the API export) are utilized.
#'   
#' @details
#'   User project access fields (those not related to forms or exports) 
#'   are mapped between coded and labeled values as:
#' 
#'   | Code | Label     |
#'   |------|-----------|
#'   | 0    | No Access |
#'   | 1    | Access    | 
#'   
#'   Form access fields are mapped as:
#'   
#'   | Code | Label                                                                 |
#'   |------|-----------------------------------------------------------------------|
#'   | 0 | No Access                                                                |
#'   | 1 | View records/responses and edit records (survey responses are read-only) |
#'   | 2 | Read Only                                                                |
#'   | 3 | Edit survey responses                                                    | 
#'   
#'   Form export permission fields are mapped as: 
#'   
#'   | Code | Label                          |
#'   |------|--------------------------------|
#'   | 0    | No Access                      |
#'   | 1    | Full Data Set                  |
#'   | 2    | De-Identified                  |
#'   | 3    | Remove Identifier Fields       |
#'   
#'   ## Importing Users/User Roles
#'   
#'   It is not required that the user provide a data frame with all of the 
#'   fields available for modification. Only fields that are provided will
#'   be modified. The only required field for imports is the `username` field.
#' 
#'   When setting permissions for a user project access fields, form access, 
#'   and form export permissions, the user may provided any of the coded 
#'   or labeled values above. The user data is passed through 
#'   [prepUserImportData()] before sending it to the API;  
#'   text values will be converted to the numeric value. 
#' 
#'   It is also permissible to use a column for each form individually, as can
#'   be exported via [exportUsers()]. With `consolidate = TRUE`, these 
#'   settings will be consolidated into the text string expected by the API. 
#' 
#'   The REDCap API does not natively allow for modifying the rights of a user
#'   that is part of a User Role. When an attempt to modify the rights of a 
#'   user in a User Role is made with this package, the user will be removed
#'   from the User Role, the rights modified, and then the User Role restored. 
#'   This is done silently: be aware that modifications to a user's rights 
#'   may not have an impact while the User Role is assigned.
#'   
#' ### Limitations 
#' 
#'   When importing via CSV, (as redcapAPI does by default) it appears that 
#'   the form access rights are imported but may not always be reflected in 
#'   the exported values. The form export rights do not appear to be imported
#'   when using the CSV format. We may be able to resolve this in the future
#'   using a JSON format.
#' 
#' @return 
#' `exportUsers` returns a data frame with the columns:
#' 
#' |            |                                                             |
#' |------------|-------------------------------------------------------------|
#' | `username` | The unique username for a user that can access the project. |
#' | `email`    | The e-mail address associated with the user in the REDCap system. |
#' | `firstname`| The user's first name.                                      |
#' | `lastname` | The user's last name.                                       |
#' | `expiration` | The date at which the user's access to the project will expire. |
#' | `data_access_group` | The text name of the Data Access Group to which the user is assigned. |
#' | `data_access_group_id` | The REDCap assigned unique identifier of the Data Access Group.|
#' | `design`   | Boolean flag indicating if the user has permissions to utilize the project design modules. |
#' | `alerts`   | Boolean flag indicating if the user has permissions to utlize the alerts tools.|
#' | `user_right` | Boolean flag indicating if the user has permissions to modify user rights. |
#' | `data_access_groups` | Boolean flag indicating if the user has user has permission to assign user to Data Access Groups.|
#' | `reports`  | Boolean flag indicating if the user has permissions to design reports. |
#' | `stats_and_charts` | Boolean flag indicating if the user has permissions to view the Statistics and Charts module.|
#' | `manage_survey_participants` | Boolean flag indicating if the user has permissions to manage survey participants.|
#' | `calendar` | Boolean flag indicating if the user has permissions to utilize the project calendar module.|
#' | `data_import_tool` | Boolean flag indicating if the user has permissions to use the data import tool.|
#' | `data_comparison_tool` | Boolean flag indicating if the user has permissions to use the data comparison tool.|
#' | `logging`  | Boolean flag indicating if the user has permissions to view the project logs (audit trail).|
#' | `file_repository` | Boolean flag indicating if the user has permissions to access the project file repository.|
#' | `data_quality_create` | Boolean flag indicating if the user has permission create new data quality rules. |
#' | `data_quality_execute` | Boolean flag indicating if the user has permission to execute data quality rules.|
#' | `api_export` | Boolean flag indicating if the user has API export privileges. |
#' | `api_import` | Boolean flag indicating if the user has API import privileges.|
#' | `mobile_app` | Boolean flag indicating if the user has permissions to use the mobile app.|
#' | `mobile_app_download_data` | Boolean flag indicating if the user has permissions to download data on the mobile app.|
#' | `record_create` | Boolean flag indicating if the user has permission to create new records.|
#' | `record_rename` | Boolean flag indicating if the user has permission to rename existing records. |
#' | `record_delete` | Boolean flag indicating if the user has permission to delete records.  |
#' | `lock_records_all_forms` | Boolean flag indicating if the user has permission to lock records across all forms. |
#' | `lock_records` | Boolean flag indicating if the user has permission to lock a records on individual forms.  |
#' | `lock_records_customization` | Boolean flag indicating if the user has permission to customize record locking. |
#' | `random_setup` | Boolean flag indicating if the user has permission to set up randomization rules. |
#' | `random_dashboard` | Boolean flag indicating if the user has permission to view the randomization dashboard.|
#' | `random_perform` | Boolean flag indicating if the user has permission to perform record randomization.|
#' | `forms` | Character string listing form access rights for each form.     |
#' | `forms_export` | Character string listing the form export rights for each form.|
#'                
#' When `form_rights = TRUE`, additional columns are created that give the
#' form access and form export rights in an individual column for each form. 
#' Form access rights columns have the naming pattern `[form_name]_access`
#' and the form export rights columns have the naming pattern
#' `[form_name]_export_access`.
#' 
#' `importUsers` invisibly returns the number of users that were added or modified.
#' 
#' `deleteUsers` invisibly returns the number of users that were deleted. 
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' # Export users
#' exportUsers(rcon)
#' 
#' # Export users without additional form access variables
#' exportUsers(rcon, 
#'             form_rights = FALSE)
#'             
#' # Export users as raw data
#' exportUsers(rcon, 
#'             labels = FALSE)
#'             
#'             
#' # Import new permissions
#' NewData <- data.frame(username = "target_user", 
#'                       design = 0, 
#'                       api_export = 1, 
#'                       api_import = "No Access")
#' importUsers(rcon, 
#'             data = NewData)
#'             
#' 
#' # Remove a user from a project
#' deleteUsers(rcon, 
#'             users = "target_user")
#' }
#' 
#' @seealso 
#' [exportUserRoles()], \cr
#' [importUserRoles()], \cr
#' [deleteUserRoles()], \cr
#' [exportUserRoleAssignments()], \cr
#' [importUserRoleAssignments()]
#' 
#' @usage NULL
#' @order 0

userMethods <- function(rcon, 
                        dates, 
                        labels, 
                        form_rights, 
                        users, 
                        data, 
                        consolidate,
                        ...)
{
  NULL
}

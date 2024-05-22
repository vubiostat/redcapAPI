#' @name userRoleMethods
#' @title Export, Import, or Delete User Roles in a Project
#' 
#' @description These methods enable the user to export user roles, 
#'   add user roles, or remove user roles from a project. 
#'   They also enable the user to modify the permissions granted to a
#'   user.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @inheritParams userMethods
#' @param data `data.frame`. Provides the user data for import. It must have
#'   a column titled `unique_role_name`. All other columns are optional.
#' @param user_roles `character`. Unique role names to be deleted from
#'   the project. 
#'   
#' @inherit userMethods details
#' 
#' @return 
#' `exportUserRoles` returns a data frame with the columns:
#' 
#' |            |                                                             |
#' |------------|-------------------------------------------------------------|
#' | `unique_role_name` | The REDCap assigned unique role name.               |
#' | `role_label` | The user provided label describing the role.              |
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
#' `importUserRoles` invisibly returns the number of user roles that were 
#'   added or modified.
#' 
#' `deleteUserRoles` invisibly returns the number of user roles that were deleted.
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' # Export users-roles
#' exportUserRoles(rcon)
#' 
#' # Export user-roles without additional form access variables
#' exportUsersRoles(rcon, 
#'                  form_rights = FALSE)
#'             
#' # Export users as raw data
#' exportUserRoles(rcon, 
#'                 labels = FALSE)
#'             
#'             
#' # Import new permissions
#' NewData <- data.frame(unique_role_name = "KN439U", 
#'                       design = 0, 
#'                       api_export = 1, 
#'                       api_import = "No Access")
#' importUserRoles(rcon, 
#'                 data = NewData)
#'             
#' 
#' # Remove a user from a project
#' deleteUserRoles(rcon, 
#'                 user_roles = "KN439U")
#' } 
#' 
#' @seealso 
#' [exportUsers()], \cr
#' [importUsers()], \cr
#' [deleteUsers()], \cr
#' [exportUserRoleAssignments()], \cr
#' [importUserRoleAssignments()]
#' 
#' @usage NULL
#' @order 0

userRoleMethods <- function(rcon, 
                            labels, 
                            form_rights, 
                            user_roles, 
                            data, 
                            consolidate,
                            ...)
{
  NULL
}

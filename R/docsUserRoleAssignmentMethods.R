#' @name userRoleAssignmentMethods
#' @title Export or Import User-Role Assignments
#'
#' @description These methods enable the user to export the user-role 
#'   assignments, add assignments, or modify existing assignments. 
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param data `data.frame` with columns `username` and 
#'   `unique_role_name`. Each `username` must be unique. 
#'   Users without a `unique_role_name` will not be assigned to 
#'   a user role.
#'   
#' @return 
#' `exportUserRoleAssignments` returns a data frame with the columns:
#' 
#' |                      |                                                      |
#' |----------------------|------------------------------------------------------|
#' | `username`           | Username of a user in the project.                   | 
#' | `unique_role_name`   | The unique role name to which the user is assigned.  |
#' | `data_access_group`  | The Data Access Group to which the user is assigned. |
#' 
#' `importUserRoleAssignments` invisibly returns the number of user roles 
#'   assignments added or modified.
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' # Export user-role assignments
#' exportUserRoleAssignments(rcon)
#' 
#' # Import/modify a user-role assignment
#' NewData <- data.frame(username = "desired_user_name", 
#'                       unique_role_name = "KN3430U")
#' importUserRolesAssignments(rcon, 
#'                            data = NewData)
#' }
#' 
#' @seealso 
#' [exportUsers()], \cr
#' [importUsers()], \cr
#' [deleteUsers()], \cr
#' [exportUserRoles()], \cr
#' [importUserRoles()], \cr
#' [deleteUserRoles()]
#' 
#' @usage NULL
#' @order 0

userRoleAssignmentMethods <- function(rcon, 
                                      data,
                                      ...)
{
  NULL
}

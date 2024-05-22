#' @name dagAssignmentMethods
#' @title Export and Import Users Assigned to Data Access Groups
#' 
#' @description These methods enable the user to export existing assignments 
#'   of users to Data Access Groups, or import new or updated assignments 
#'   to the project. 
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param data `data.frame` with the columns `username` and 
#'   `redcap_data_access_group`. The should only be one row per 
#'   user name.
#' 
#' @details When modifying existing assignments using the import method, 
#'   the user must provide the
#'   unique user name and the group name. If the `redcap_data_access_group`
#'   column is not provided, the REDCap user will not be assigned to any group. 
#' 
#' @return 
#' `exportUserDagAssignments` method returns a data frame with two columns:
#' 
#' |            |                                                    |
#' |------------|----------------------------------------------------|
#' | `username` | The unique user name for each user in the project. | 
#' | `redcap_data_access_group` | The unique Data Access Group name to which the user is assigned. |
#' 
#' `importUserDagAssignments` invisibly returns the number of assignments imported.
#'   
#' @seealso 
#' [exportDags()],\cr
#' [importDags()],\cr
#' [deleteDags()],\cr
#' [switchDag()]
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
#' @usage NULL
#' @order 0
# dummy function to control the order of arguments in the help file.
dagAssignmentMethods <- function(rcon, 
                                 data,
                                 ...)
{
  NULL
}

#' @name dagMethods
#' @title Export, Import, Delete Data Access Groups from a Project
#' 
#' @description These methods enable the user to export existing Data Access Groups, 
#'   import new Data Access Groups, or delete Data Access Groups from a 
#'   project.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param data A `data.frame` with two columns: `data_access_group_name`
#'   and `unique_group_name`. 
#' @param dags `character` vector of names matching the `unique_group_name`.
#'   
#' @details To import new data access groups, the user must provide a value for 
#'   `data_access_group_name` with no value (`NA`) for `unique_group_name`. 
#'   
#'   To modify a group name, provide a new value for `data_access_group_name`
#'   with the associated `unique_group_name`. If `unique_group_name`
#'   is provided, it must match a value currently in the project.
#'   
#' @return 
#' `exportDags` with the columns
#' 
#' |                          |                                                    |
#' |--------------------------|----------------------------------------------------|
#' | `data_access_group_name` | The human readable name for the data access group. |
#' | `unique_group_name`      | The internal unique group name.                    |
#' | `data_access_group_id`   | The internal numeric identifier.                   |
#' 
#' `importDags` invisibly returns the number of Data Access Groups imported. 
#' 
#' `deleteDags` invisibly returns the number of Data Access Groups deleted.
#' 
#'
#' @seealso
#' [switchDag()],\cr
#' [exportUserDagAssignments()], \cr
#' [importUserDagAssignments()]
#' 
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
#' @usage NULL
#' @order 0
# dummy function to control the order of arguments in the help file.
dagMethods <- function(rcon, 
                       dags, 
                       data, 
                       ...)
{
  NULL
}

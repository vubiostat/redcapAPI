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

NULL

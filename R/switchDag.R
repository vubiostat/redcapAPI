#' @name switchDag
#' @title Switch Data Access Group Assignment for the Current User
#' 
#' @description This method enables the current API user to switch 
#'   (assign/reassign/unassign) their current Data Access Group assignment 
#'   if they have been assigned to multiple DAGs via the DAG Switcher page 
#'   in the project. 
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param dag `character(1)` A unique data access group to which to 
#'   assign the current user. Use `NA` to leave the user unassigned.
#'   
#' @return Invisibly returns `TRUE` when the call is completed successfully.
#'   Otherwise an error is thrown.
#' 
#' @seealso 
#' [exportDags()],\cr
#' [importDags()],\cr
#' [deleteDags()], \cr
#' [exportUserDagAssignments()], \cr
#' [importUserDagAssignments()]
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
#' @usage NULL
#' @order 0
# dummy function to control the order of arguments in the help file.
switchDagArgs <- function(rcon, 
                          dag,
                          ..., 
                          error_handling, 
                          config, 
                          api_param){
  NULL
}

#' @rdname switchDag
#' @export

switchDag <- function(rcon, 
                      dag, 
                      ...){
  UseMethod("switchDag")
}

#' @rdname switchDag
#' @export

switchDag.redcapApiConnection <- function(rcon, 
                                          dag,
                                          ...,
                                          error_handling = getOption("redcap_error_handling"), 
                                          config         = list(), 
                                          api_param      = list()){
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapConnection", 
                          add = coll)
  
  checkmate::assert_character(x = dag, 
                              len = 1, 
                              null.ok = FALSE, 
                              any.missing = TRUE, 
                              add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling, 
                                        choices = c("null", "error"), 
                                        .var.name = "error_handling",
                                        add = coll)
  
  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         names = "named", 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  checkmate::assert_subset(x = dag, 
                           choices = c(rcon$dags()$unique_group_name, NA, NA_character_), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # API Body List
  
  if (is.na(dag)) dag <- ""
  
  body <- list(content = "dag", 
               action = "switch", 
               dag = dag)
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Make the API Call
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  rcon$flush_dag_assignment()
  
  if (response$status_code != 200) return(redcapError(response, error_handling))

  success <- isTRUE(as.character(response) == "1")
  
  if (!success) {
    message(as.character(response))
  }
  
  invisible(success)
}

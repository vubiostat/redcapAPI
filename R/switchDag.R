#' @name switchDag
#' @title Switch Data Access Group Assignment for the Current User
#' 
#' @description This method allows the current API user to switch 
#'   (assign/reassign/unassign) their current Data Access Group assignment 
#'   if they have been assigned to multiple DAGs via the DAG Switcher page 
#'   in the project. 
#'   
#' @param rcon \code{redcapConnection} object.
#' @param dag \code{character(1)} A unique data access group to which to 
#'   assign the current user. Use \code{NA} to leave the user unassigned.
#' @param refresh \code{logical(1)} If \code{TRUE}, the cached data access
#'   group assignments will be refreshed.
#' @param ... Additional arguments to pass to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcapError}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'   
#' @return Returns \code{TRUE} when the call is completed successfully.
#' 
#' @seealso 
#' \code{\link{exportDags}},
#' \code{\link{importDags}},
#' \code{\link{deleteDags}}, 
#' \code{\link{exportUserDagAssignments}},
#' \code{\link{importUserDagAssignments}}
#'   
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
                                          refresh = TRUE, 
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
  
  checkmate::assert_logical(x = refresh, 
                            len = 1, 
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
  
  if (response$status_code != 200) return(redcapError(response, error_handling))

  success <- isTRUE(as.character(response) == "1")
  
  if (refresh){
    rcon$refresh_dag_assignment()
  }
  
  if (!success) {
    message(as.character(response))
  }
  
  success
}

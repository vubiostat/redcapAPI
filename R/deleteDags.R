#' @name dagsMethods
#' @aliases deleteDags, exportDags, importDags
#' 
#' @title Export, Import, Delete Data Access Groups from a Project
#' 
#' @description These methods allow you to export existing Data Access Groups, 
#'   import new Data Access Groups, or delete Data Access Groups from a 
#'   project.
#' 
#' @param rcon A \code{redcapConnection} object.
#' @param data A \code{data.frame} with two columns: \code{data_access_group_name}
#'   and \code{unique_group_name}. 
#' @param dags \code{character} vector of names matching the \code{unique_group_name}.
#' @param refresh \code{logical(1)}. When \code{TRUE}, cached event data will 
#'   be refreshed after the import.
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
#' @details When importing data access groups, provide a value for \code{data_access_group_name} 
#'   with no value for \code{unique_group_name}. 
#'   
#'   To modify a group name, provide a new value for \code{data_access_group_name}
#'   with the associated \code{unique_group_name}. If \code{unique_group_name}
#'   is provided, it must match a value currently in the project.
#'
#' @seealso
#' \code{\link{switchDag}},
#' \code{\link{exportUserDagAssignments}}, 
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
#' @export

deleteDags <- function(rcon, 
                       dags, 
                       ...){
  UseMethod("deleteDags")
}

#' @rdname dagsMethods
#' @export

deleteDags.redcapApiConnection <- function(rcon, 
                                           dags,
                                           refresh = TRUE,
                                           ..., 
                                           error_handling = getOption("redcap_error_handling"), 
                                           config         = list(), 
                                           api_param      = list()){
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_character(x = dags, 
                              min.len = 1,
                              any.missing = FALSE,
                              add = coll)
  
  checkmate::assert_logical(x = refresh, 
                            len = 1, 
                            null.ok = FALSE, 
                            any.missing = FALSE, 
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
  
  
  
  checkmate::assert_subset(dags, 
                           choices = rcon$dags()$unique_group_name, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make the API Body list                                       ####
  
  body <- c(list(content = "dag",
                 action = "delete"),
            vectorToApiBodyList(dags, "dags"))
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API                                                 ####
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) return(redcapError(response, error_handling))
  
  if (refresh){
    rcon$refresh_dags()
  }
  
  message(sprintf("DAGs deleted: %s", 
                  as.character(response)))
}

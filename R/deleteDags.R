#' @name deleteDags
#' @title Delete Data Access Groups from a Project
#' 
#' @description This method allows you to delete DAGs from a project.
#' 
#' @param rcon A \code{redcapConnection} object.
#' @param dags \code{character} vector of names matching the \code{unique_group_name}.
#' @param refresh \code{logical(1)}. When \code{TRUE}, cached event data will 
#'   be refreshed after the import.
#' @param ... Additional arguments to pass to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'   
#' @export

deleteDags <- function(rcon, 
                       dags, 
                       ...){
  UseMethod("deleteDags")
}

#' @rdname deleteDags
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

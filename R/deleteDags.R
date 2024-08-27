# Complete documentation in documentation.R
#' @describeIn dagMethods Delete Data Access Groups from a project.
#' @order 3
#' @export

deleteDags <- function(rcon, 
                       dags, 
                       ...){
  UseMethod("deleteDags")
}

#' @rdname dagMethods
#' @order 6
#' @export

deleteDags.redcapApiConnection <- function(rcon, 
                                           dags,
                                           ...)
{
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

  ###################################################################
  # Call the API                                                 ####
  rcon$flush_dags()
  invisible(as.character(makeApiCall(rcon, body, ...)))
}

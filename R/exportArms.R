#' @describeIn armsMethods Export the arms defined in a project.
#' @order 1
#' @export

exportArms <- function(rcon, ...){
  UseMethod("exportArms")
}

#' @rdname armsMethods
#' @order 4
#' @export

exportArms.redcapApiConnection <- function(rcon, 
                                           arms = character(0), 
                                           ...)
{
  if (is.numeric(arms)) arms <- as.character(arms)
  
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  checkmate::assert_character(x = arms,
                              add = coll)

  checkmate::reportAssertions(coll)
  
  if(rcon$projectInformation()$is_longitudinal == 0){
    return(REDCAP_ARMS_STRUCTURE) # defined in redcapDataStructure.R
  }
  
  # Build the body list ---------------------------------------------
  body <- c(list(content = 'arm', 
                 format = 'csv', 
                 returnFormat = 'csv'), 
            vectorToApiBodyList(arms, 
                                parameter_name = "arms"))

  # API Call --------------------------------------------------------
  as.data.frame(makeApiCall(rcon, body, ...))
}

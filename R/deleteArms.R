#' @describeIn armsMethods Delete arms from a project.
#' @order 3
#' @export

deleteArms <- function(rcon, 
                       arms, 
                       ...){
  UseMethod("deleteArms")
}


#' @rdname armsMethods
#' @order 6
#' @export

deleteArms.redcapApiConnection <- function(rcon, 
                                           arms,
                                           ...)
{
  if (is.numeric(arms)) arms <- as.character(arms)

   ##################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(arms, 
                              any.missing = FALSE,
                              add = coll)

  checkmate::reportAssertions(coll)
  
  Arms <- rcon$arms()
  
  checkmate::assert_subset(x = arms, 
                           choices = as.character(Arms$arm_num), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make API Body List
  body <- c(list(content = "arm",
                 action = "delete"),
            vectorToApiBodyList(arms, "arms"))

  ###################################################################
  # Call the API
  invisible(
    if (length(arms) > 0)
    { # Skip the call if there are no arms to delete
      rcon$flush_arms()
      rcon$flush_events()
      rcon$flush_projectInformation()
      as.character(makeApiCall(rcon, body, ...))
    } else
    {
      "0"
    }
  )
}

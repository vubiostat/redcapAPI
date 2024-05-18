#' @describeIn armsMethods Import and modify the arms definitions in a project.
#' @order 2
#' @export

importArms <- function(rcon, 
                       data, 
                       override = FALSE, 
                       ...){
  UseMethod("importArms")
}

#' @rdname armsMethods
#' @order 5
#' @export

importArms.redcapApiConnection <- function(rcon, 
                                           data,  
                                           override       = FALSE,
                                           ...)
{
  dots <- list(...)
  if ("arms_data" %in% names(dots)) data <- dots$arms_data
  
  ##################################################################
  # Argument Validation 
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_data_frame(x = data,
                               ncols = 2,
                               add = coll)
  
  checkmate::assert_logical(x = override,
                            len = 1,
                            add = coll)

  checkmate::reportAssertions(coll)
  
  checkmate::assert_subset(x = names(data),
                           choices = c("arm_num", "name"),
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  checkmate::assert_integerish(data[["arm_num"]],
                               add = coll,
                               .var.name = "data$arm_num")
  
  checkmate::assert_character(data[["name"]],
                              add = coll,
                              .var.name = "data$name")
  
  checkmate::reportAssertions(coll)
      
   ##################################################################
  # Make API Body List
  
  body <- list(content = "arm",
               override = as.numeric(override),
               action = "import",
               format = "csv",
               data = writeDataForImport(data))

   ##################################################################
  # API Call
  rcon$flush_arms()
  # Changes to arms can impact events and if the project is 
  # still considered longitudinal
  rcon$flush_events()
  rcon$flush_projectInformation()  
  invisible(as.character(
    makeApiCall(rcon, body, ...)
  ))
}

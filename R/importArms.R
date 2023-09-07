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
                                           refresh        = TRUE,
                                           ...,
                                           error_handling = getOption("redcap_error_handling"), 
                                           config         = list(), 
                                           api_param      = list()){
  
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
  
  body <- list(token = rcon$token,
               content = "arm",
               override = as.numeric(override),
               action = "import",
               format = "csv",
               data = writeDataForImport(data))
  
  body <- body[lengths(body) > 0]
  
   ##################################################################
  # API Call
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) return(redcapError(response, error_handling))
  
  message(sprintf("Arms imported: %s", 
                  as.character(response)))
  
  if (refresh && rcon$has_arms()){
    rcon$refresh_arms()
    rcon$refresh_projectInformation()
  }
}

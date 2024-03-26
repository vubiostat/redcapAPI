
exportDataQuality <- function(rcon, prefix,
                                     ...){
  UseMethod("exportDataQuality")
}

exportDataQuality.redcapApiConnection <- function(rcon, prefix,
                                                       ..., 
                                                       error_handling = getOption("redcap_error_handling"), 
                                                       config = list(), 
                                                       api_param = list()){
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_class(x = prefix, 
                          classes = "character", 
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
  
  ###################################################################
  # Build the query list                                         ####
  
  url <- paste0(rcon$url, "?prefix=", prefix, "&page=export&type=module&NOAUTH&pid=", rcon$projectInformation()$project_id)
  
  formData <- list(token = rcon$token)
  
  response <- httr::POST(url, body = formData, encode = "form")
  tryCatch({
    result <- httr::content(response, type = 'application/json')
    
    for(j in 1:length(result)){i=result[[j]];if(is.null(i$resolutions)){result[[j]]$resolutions=list()}}
    result <- as.data.frame(do.call(rbind, result))
    
    if (nrow(result) > 0) {
      columns <- c("status_id", "project_id", "record", "event_id", "instance", "field_name")
      for (c in columns) {
        result[, c] <- unlist(result[, c])
      }

      result$codes <- lapply(result$resolutions, function(rs) {
        rs_df <- as.data.frame(do.call(rbind, rs))
        m <- stringr::str_match(unlist(rs_df$comment), "^([a-zA-Z]+\\d+):\\s")[, 2]
        m <- m[!is.na(m)]
        return(m)
      })
    }
    
    return(result)
  }, stop ("Error in result: Make sure the Data Quality API module is enabled in your project.")
  )
  
  
  
  ###################################################################
  # Make the API Call                                            ####
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (as.character(response) == ""){
    return(REDCAP_DQ_STRUCTURE)
  }
  
  as.data.frame(response)
}

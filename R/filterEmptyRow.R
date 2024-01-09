#' @name filterEmptyRow
#' @title Remove Rows Containing Only Missing Values
#' 
#' @description Evaluates each row of a data frame for missingness. If all
#' fields (excluding the identifying fields) are missing, the row is 
#' removed from the data. For the purpose of this function, 
#' `redcap_data_access_group` is considered an identifying field.
#' 
#' @inheritParams common-rcon-arg
#' @param data A `data.frame` to be filtered.
#' 
#' @seealso 
#' [exportRecordsTyped()], \cr
#' [exportReportsTyped()]

filterEmptyRow <- function(data, 
                           rcon){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_data_frame(x = data, 
                               add = coll)
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapConnection", 
                          add = coll)
  
  checkmate::reportAssertions(coll)
  
  invalid <- attr(data, "invalid")
  
  id_fields <- c(getProjectIdFields(rcon), REDCAP_SYSTEM_FIELDS,
                 "redcap_data_access_group")
  
  other_fields <- setdiff(names(data), id_fields)
  
  if(length(other_fields) == 0)
    NewData <- data
  
  else {
    is_all_missing <- apply(is.na(data[other_fields]), 1, all)
    
    NewData <- data[!is_all_missing, , drop = FALSE]
    
    for (field in names(NewData)) {
      attributes(NewData[[field]]) <- attributes(data[[field]])
    }
    
    attr(NewData, "invalid") <- invalid
  }
  
  NewData
}

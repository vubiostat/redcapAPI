#' @name filterEmptyRow
#' @title Remove Rows Containing Only Missing Values
#' 
#' @description Evaluates each row of a data frame for missingness. If all
#' fields (excluding the identifying fields) are missing, the row is 
#' removed from the data. For the purpose of this function, 
#' \code{redcap_data_access_group} is considered an identifying field.
#' 
#' @param data A \code{data.frame} to be filtered.
#' @param rcon A \code{redcapConnection} object.
#' 

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
  
  id_fields <- c(getProjectIdFields(rcon), 
                 REDCAP_SYSTEM_FIELDS, 
                 "redcap_data_access_group")
  
  is_all_missing <- logical(nrow(data))
  
  for (i in seq_along(is_all_missing)){
    this_row <- data[ i, names(data)[!names(data) %in% id_fields], drop = FALSE]
    if (ncol(this_row) == 0){ # This occurs when all of the columns are ID fields.
      is_all_missing[i] <- FALSE
    } else if (all(is.na(unlist(this_row)))){
      is_all_missing[i] <- TRUE
    }
  }
  
  NewData <- data[!is_all_missing, , drop = FALSE]
  
  for (field in names(NewData)){
    attributes(NewData[[field]]) <- attributes(data[[field]])
  }
  
  attr(NewData, "invalid") <- invalid
  
  NewData
}
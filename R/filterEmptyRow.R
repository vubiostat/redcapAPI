  #####################################################################
 ##
## Helper function to filter a data.frame and preserve all attributes
##
.df_filter_i <- function (x, i)
{
  rows  <- attr(x, "row.names")[i]
  attrs <- lapply(x, attributes)
  x     <- unclass(x)
  for (j in seq_along(x))
  {
    x[[j]] <- x[[j]][i]
    attributes(x[[j]]) <- attrs[[j]]
  }
  class(x) <- 'data.frame'
  attr(x, "row.names") <- rows
  x
}

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
  
  other_fields <- setdiff(names(data),
                          c(getProjectIdFields(rcon),
                            REDCAP_SYSTEM_FIELDS,
                            "redcap_data_access_group"))
  
  if(length(other_fields) == 0)
    data
  else
  {
    has_any_value <- rowSums(!is.na(data[other_fields])) != 0
    if(all(has_any_value))
      data
    else
      .df_filter_i(data, has_any_value)
  }
}



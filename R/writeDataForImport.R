#' @name writeDataForImport
#' @title Prepare a Data Frame for Import Through the API
#' 
#' @description Converts a dataframe into a character value in the format
#'   of a CSV for import through the API.
#'   
#' @param data \code{data.frame} to be imported to the API
#' 

writeDataForImport <- function(data){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_data_frame(x = data, 
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  output <- 
    utils::capture.output(
      utils::write.csv(data,
                       file = "",
                       na = "",
                       row.names = FALSE)
    )
  
  paste0(output, collapse = "\n")
}
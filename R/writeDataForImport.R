#' @name writeDataForImport
#' @title Prepare a Data Frame for Import Through the API
#' 
#' @description Converts a dataframe into a character value in the format
#'   of a CSV for import through the API.
#'   
#' @param data `data.frame` to be imported to the API
#' @param csv_delimiter `character(1)` Delimiter used to separate fields in the generated CSV
#'   string. Defaults to `","`.
#'

writeDataForImport <- function(data, csv_delimiter = ","){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_data_frame(x = data, 
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  output <- 
    utils::capture.output(
      utils::write.table(data,
                         file = "",
                         sep = csv_delimiter,
                         na = "",
                         row.names = FALSE,
                         col.names = TRUE,
                         qmethod = "double")
    )
  
  paste0(output, collapse = "\n")
}

#' @name widerRepeated
#' @title Transform Data Into Wide Format
#' 
#' @description Converts a dataframe into wide format given the column names to expand.
#'   
#' @param Records A \code{data.frame} containing the records from
#'        \code{\link{exportRecordsTyped}}
#' @param idvar \code{character}, Names of one or more variables in long format that identify multiple records belonging to the same group. 
#'        These variables may also be present in wide format. (e.g., record_id)
#' @param repeat_var \code{character}, Variable in long format that that differentiates multiple records belonging to the same group. (e.g., med01_name).  
#'        If more than one record matches, the first will be taken (with a warning).

widerRepeated <- function(Records, idvar, repeat_var){
  Records <- reshape(Records, idvar = idvar, timevar = timevar, direction = 'wide')
}

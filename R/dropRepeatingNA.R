#' @name dropRepeatingNA
#' @title Drop Row Where Repeat Instrument Is NA
#' 
#' @description Drops rows where the repeat instrument is NA. Returns a data frame of records where repeat instruments have a value.
#'   
#' @param Records A \code{data.frame} containing the records from
#'        \code{\link{exportRecordsTyped}}
#' @param rcon A REDCap connection object as created by \code{redcapConnection}.
#' @param quiet \code{logical(1)}. When \code{FALSE}, a message is printed
#'   indicating how many rows were in \code{Records} at the start and 
#'   completion of the subset.

dropRepeatingNA <- function(Records, rcon, quiet=FALSE)
{
  if('redcap_repeat_instrument' %in% names(Records))
  {
    x <- Records$redcap_repeat_instrument
    
    if(any(is.na(x)))
    {
      if(!quiet) 
        message(
          paste('Project', rcon$projectInformation()$project_title,
                'had', length(x), 'rows, subsetted to',
                sum(! is.na(x)), 'rows'))
      
      return(Records[! is.na(x),])
    } 
  }
  
  Records
} 
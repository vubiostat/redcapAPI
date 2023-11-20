#' @name changedRecords
#' @title get a list of records changed (adds, updates, deletes)
#' 
#' @description This is a convience function that scans logs and 
#' returns record ids of changes. 
#' 
#' @param rcon A redcapConnection object.
#' @param ... Arguments passed to `exportLogging`
#' 
#' @details Makes a call to `exportLogging` and 
#'   
#' @return 
#' Returns a list with the elements
#' 
#' |           |                                          |
#' |-----------|------------------------------------------|
#' | `updated` | character vector of updated record's ids |
#' | `deleted` | charcater vector of deleted record's ids |
#' | `created` | character vector of created record's ids |

#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'
#' # Changes in last 24 hours     
#' changedRecords(rcon, beginTime=as.POSIXct(Sys.time()-86400))
#' }
#' 
#' @export

changedRecords <- function(rcon,
                           ...)
{
  # Relies on exportLogging for argument validation 
  Log <- exportLogging(rcon, ...)

  list(
    updated = unique(Log$record[grepl("^update record", 
                                      Log$action, 
                                      ignore.case = TRUE)]), 
    deleted = unique(Log$record[grepl("^delete record", 
                                      Log$action, 
                                      ignore.case = TRUE)]),
    created = unique(Log$record[grepl("^create record", 
                                      Log$action, 
                                      ignore.case = TRUE)])
  )
}
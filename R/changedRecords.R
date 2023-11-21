#' @name changedRecords
#' @title returns a list of record IDs changed (adds, updates, deletes)
#' 
#' @description This is a convenience function that scans logs and 
#' returns record IDs of changed records.  
#' 
#' @inheritParams common-rcon-arg
#' @param ... Arguments passed to [exportLogging()]
#' 
#' @details Makes a call to `exportLogging` with passed arguments. Returns 
#' filtered list or records IDs with update, delete or create events.
#'   
#' @return 
#' Returns a list with the elements
#' 
#' |           |                                          |
#' |-----------|------------------------------------------|
#' | `updated` | character vector of updated record IDs |
#' | `deleted` | character vector of deleted record IDs |
#' | `created` | character vector of created record IDs |

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

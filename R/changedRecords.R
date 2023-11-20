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
  Log <- exportLogging(rcon, ...)
  
  create_record <- which(grepl("^create record", 
                               Log$action, 
                               ignore.case = TRUE))
  
  delete_record <- which(grepl("^delete record", 
                               Log$action, 
                               ignore.case = TRUE))
  
  update_record <- seq_len(nrow(Log))
  update_record <- update_record[!update_record %in% c(create_record, 
                                                       delete_record)]
  list(
    updated = unique(Log$record[update_record]), 
    deleted = unique(Log$record[delete_record]),
    created = unique(Log$record[create_record])
  )
}
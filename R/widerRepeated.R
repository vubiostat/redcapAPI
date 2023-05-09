#' @name widerRepeated
#' @title Transform Data Into Wide Format
#' 
#' @description Converts a dataframe into wide format given a single REDCap form.
#' This function assumes that the Records argument is the result of exportRecordsTyped,
#' and that all empty values have been previously dropped.
#'   
#' @param Records A \code{data.frame} containing the records from
#'        \code{\link{exportRecordsTyped}}
#' @param rcon A REDCap connection object as created by \code{unlockREDCap}.
#' @export
widerRepeated <- function(Records, rcon)
{
  idvar <- rcon$metadata()$field_name[1]
  ###########################################################################
  # Check parameters passed to function
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_data_frame(x = Records)
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  checkmate::assert_character(x = idvar,
                              len = 1,
                              add = coll)
  checkmate::assert_subset(x = idvar, names(Records))
  checkmate::reportAssertions(coll)
  
  # check that redcap_repeat_instrument is present and that all values are the same
  if ("redcap_repeat_instrument" %in% colnames(Records) && all(!is.na(Records$redcap_repeat_instrument))) {
    if (length(unique(Records$redcap_repeat_instrument)) == 1) {
      cat("Great! The redcap_repeat_instrument column has unique values.\n")
      
      Records_wide <- reshape(
        data = Records,
        idvar = c(idvar, "redcap_event_name", "redcap_repeat_instrument"),
        timevar = "redcap_repeat_instance",
        direction = "wide"
      )
      return(Records_wide)
      
    } else {
      stop("The redcap_repeat_instrument column does not have unique values.\n")
    }
  } else {
    cat("The redcap_repeat_instrument column is not present.\n")
    return(Records)
  }
}


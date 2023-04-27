#' @name guessCast
#' 
#' @rdname guessCast
#' @title A helper function to make a guess at casting uncast columns
#' @description Will do a type cast if a validation is met above
#'  a threshold ratio of non-NA records. It modifies the existing
#'  \code{invalid} attribute to reflect the cast. 
#' @param Records A \code{data.frame} containing the records from
#'        \code{\link{exportRecordsTyped}}
#' @param conn The REDCap connection object. 
#' @param quiet Print no messages if triggered, Default=FALSE. 
#' @param na function. A function of the signature function(x) that determines
#' if a field is NA.
#' @param validation function. A function of the signature function(x) that
#' determines if a field is valid. 
#' @param cast function. A function of the signature function(x) that
#' casts a column passing the validation.
#' @param threshold numeric(1). The threshold of non-NA data to trigger casting.
#' @export
#' @examples
#' \dontrun{
#' recs <- exportRecordsTyped(rcon, cast=raw_cast) |> 
#'   guessCast(validation=valRx("^[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])$"), 
#'             cast=as.Date,
#'             threshold=0.6)
#' }
guessCast <- function(Records, conn, 
                      na=isNAorBlank, validation, cast,
                      quiet=FALSE, threshold=0.8)
{
  field_classes <- sapply(Records, class)
  text_fields   <- names(Records)[field_classes == "character"]
  for(i in text_fields)
  {
    nas   <- isNAorBlank(Records[[i]])
    valid <- validation(Records[[i]])
    if ( (sum(valid) - sum(nas))/length(Records[i]) >= threshold )
    {
      if(!quiet)
        message(paste0("guessCast triggered on ", i,
                       " for ", sum(valid),
                       " of ", length(valid), " records."))
      x <- Records[[i]]

      # Modify "invalid" attribute
      if(any(!valid))
      {
        inv <- attr(Records, "invalid")
        sel <- !valid & !nas
        inv <- rbind(
                 inv,
                 data.frame(row        = seq_len(nrow(Records))[sel],
                            record_id  = NA,
                            field_name = i,
                            field_type = "text",
                            value      = Records[sel, i])
        )
        attr(Records, "invalid") <- inv
      }
      
      x[ nas | !valid ] <- NA
      Records[[i]] <- cast(x)
    }
  }
  
  if(!is.null(attr(Records, "invalid")))
  {
    class(attr(Records, "invalid")) <- c("invalid", "data.frame")
    attr(attr(Records, "invalid"), "time")    <- format(Sys.Date(), "%c")
    attr(attr(Records, "invalid"), "version") <- conn$version()
    attr(attr(Records, "invalid"), "project") <- conn$projectInfo()$project_title
  }
  
  Records
}

#' @rdname guessCast
#' @export
guessDate <- function(Records,
                      conn,
                      na         = isNAorBlank,
                      validation = valRx("^[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])$"),
                      cast       = function(x, ...) as.POSIXct(x, format = "%Y-%m-%d"),
                      quiet      = FALSE,
                      threshold  = 0.8)
  guessCast(Records,  conn,
            na = na,  validation = validation, cast = cast,
            quiet = quiet,
            threshold = threshold)
                      
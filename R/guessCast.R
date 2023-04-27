#' @name guessCast
#' 
#' @title A helper function to make a guess at casting uncast columns
#' @description Will do a type cast if a validation is met above
#'  a threshold ratio of non-NA records. It modifies the existing
#'  \code{invalid} attribute to reflect the cast. 
#' @param Records A \code{data.frame} containing the records from
#'        \code{\link{exportRecordsTyped}}
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
guessCast <- function(Records, na=isNAorBlank, validation, cast, threshold=0.8)
{
  field_classes <- sapply(Records, class)
  text_fields   <- names(Records)[field_classes == "character"]
  for(i in text_fields)
  {
    nas   <- isNAorBlank(Records[i])
    valid <- validation(Records[i])
    if ( (sum(valid) - sum(na))/length(Records[i]) >= threshold )
    {
      
    }
  }
  Records
}

guessDate <- function(Records,
                      na         = isNAorBlank,
                      validation = valRx("^[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])$"),
                      cast       = function(x, ...) as.POSIXct(x, format = "%Y-%m-%d"),
                      threshold  = 0.8)
  guessCast(Records, na=na, validation=validation, cast=cast, threshold=threshold)
                      
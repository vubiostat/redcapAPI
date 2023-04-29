#' @name recastForImport
#' @title Cast Fields for Import to a REDCap Project
#' 
#' @description This is a special case recast function for transforming 
#'   a data from to be imported to a REDCap project. It operates in a 
#'   similar fashion to \code{exportRecordsTyped} where it will scan 
#'   data to determine that it is in a valid format, and provide a report
#'   of any invalid results so that they can be investigated. The
#'   result is a data frame where every column is a character vector in 
#'   a format acceptable for API import method.
#'   
#' @param data \code{data.frame} to prepare for import.
#' @param rcon A \code{redcapConnection} object.
#' @param na  A named \code{list} of user specified functions to determine if the
#'   data is NA. This is useful when data is loaded that has coding for NA, e.g.
#'   -5 is NA. Keys must correspond to a truncated REDCap field type, i.e.
#'   {date_, datetime_, datetime_seconds_, time_mm_ss, time_hh_mm_ss, time, float,
#'   number, calc, int, integer, select, radio, dropdown, yesno, truefalse,
#'   checkbox, form_complete, sql}. The function will be provided the variables
#'   (x, field_name, coding). The function must return a vector of logicals
#'   matching the input. It defaults to \code{\link{isNAorBlank}} for all
#'   entries.
#' @param validation A named \code{list} of user specified validation functions. The 
#'   same named keys are supported as the na argument. The function will be 
#'   provided the variables (x, field_name, coding). The function must return a
#'   vector of logical matching the input length. Helper functions to construct
#'   these are \code{\link{valRx}} and \code{\link{valChoice}}. Only fields that
#'   are not identified as NA will be passed to validation functions. 
#' @param cast A named \code{list} of user specified class casting functions. The
#'   same named keys are supported as the na argument. The function will be 
#'   provided the variables (x, field_name, coding). The function must return a
#'   vector of logical matching the input length. See \code{\link{fieldValidationAndCasting}}
#' 
#' @export

recastForImport <- function(data, 
                            rcon, 
                            fields     = NULL,
                            na         = list(),
                            validation = list(), 
                            cast       = list(), 
                            ...){
  
  if (is.null(fields)) fields <- names(data)
  
  ###################################################################
  # Argument Valdiation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_data_frame(x = data, 
                               col.names = "named",
                               add = coll)
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapConnection", 
                          add = coll)
  
  checkmate::assert(
    checkmate::test_character(x = fields), 
    checkmate::test_logical(x = fields), 
    checkmate::test_integerish(x = fields, lower = 0), 
    .var.name = "fields",
    add = coll
  )
  
  checkmate::assert_list(x = na, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = validation, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = cast, 
                         names = "named", 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  .exportRecordsTyped_validateFieldForm(rcon = rcon, 
                                        fields = fields, 
                                        drop_fields = NULL, 
                                        forms = NULL, 
                                        coll = coll)
  
  checkmate::reportAssertions(coll)
  
  Raw <- as.data.frame(lapply(data, 
                              function(x) trimws(as.character(x))))

  castRecords(Raw              = Raw, 
              Records          = data, 
              rcon             = rcon, 
              na               = na, 
              validation       = validation, 
              cast             = cast, 
              assignment       = NULL, 
              default_cast     = .default_cast_import, 
              default_validate = .default_validate_import)
}

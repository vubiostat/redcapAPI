#' @name fieldValidationAndCasting
#' @title Helper functions for \code{exportRecordsTyped} Validation and Casting
#' @description This set of functions assists in validating that the content of 
#'   fields coming from REDCap match the MetaData, allowing for a 
#'   validation report to provided. The cast helpers allow for transforming
#'   the REDCap data into R data types and allowing the user to customize 
#'   the end product.
#'   
#' @param x \code{character}. A vector to check.
#' @param rx \code{character}. The regular expression pattern to check.
#' @param coding named \code{character} vector. The defined coding from the meta data.
#' @param field_name \code{character(1)}. Name of the field(s)
#' @param ... Consumes anything else passed to function. I.e., field_name and 
#' coding.
#'
#' @details Functions passed to the \code{na}, \code{validation}, and
#' \code{cast} parameter of \code{\link{exportRecordsTyped}} all take the form
#' of \code{function(x, coding, field_name)}. \code{na} and \code{validation}
#' functions are expected to return a logical vector of the same length as the
#' column processed. Helper routines
#' are provided here for common cases to construct these functions. 
#' 
#' \code{isNAorBlank} returns TRUE/FALSE if field is NA or blank. Helper
#' function for constructing \code{na} overrides in \code{\link{exportRecordsTyped}}.
#' 
#' \code{valRx} constructs a validation function from a regular expression pattern. 
#' The function returns a TRUE/FALSE if the value matches the pattern.
#' 
#' \code{valchoice} constructs a validation function from a set of choices 
#' defined in the MetaData. The functions returns a TRUE/FALSE if the value
#' matches one of the choices.
#' 
#' \code{castLabel} constructs a casting function for multiple choice variables. 
#' The field will be cast to return the choice label (generally more human readable)
#' 
#' \code{castCode} constructs a casting function for multiple choice variables.
#' Similar to \code{castLabel}, but the choice value is returned instead. The
#' values are typically more compact and their meaning may not be obvious.
#' 
#' \code{castRaw} constructs a casting function that returns the content
#' from REDCap as it was received. It is functionally equivalent to \code{identity}
#' 
#' \code{castChecked} constructs a casting function for checkbox fields. It
#' returns values in the form of Unchecked/Checked.
#' 
#' \code{castCheckLabel} and \code{castCheckCode} also construct casting functions
#' for checkbox fields. For both, unchecked variables are cast to an empty 
#' string (""). Checked variables are cast to the option label and option code, 
#' respectively.
#' 
#' \code{raw_cast} overrides all casting if passed as the \code{cast}
#' parameter.
#' 
#' @author Shawn Garbett, Benjamin Nutter
#' 
#' @export
isNAorBlank <- function(x, ...) is.na(x) | x==''

#' @rdname fieldValidationAndCasting
#' @export
valRx <- function(rx) { function(x, ...) grepl(rx, x) }

#' @rdname fieldValidationAndCasting
#' @export
valChoice <- function(x, field_name, coding) grepl(paste0(coding,collapse='|'), x)

#' @rdname fieldValidationAndCasting
#' @export
castLabel      <- function(x, coding, field_name) factor(x, levels=coding, labels=names(coding))

#' @rdname fieldValidationAndCasting
#' @export
castCode       <- function(x, coding, field_name) factor(x, levels=coding, labels=coding)

#' @rdname fieldValidationAndCasting
#' @export
castRaw        <- function(x, coding, field_name) x

#' @rdname fieldValidationAndCasting
#' @export
castChecked    <- function(x, coding, field_name) factor(c("Unchecked", "Checked")[(x=='1'|x=='yes')+1], levels=c("Unchecked", "Checked"))

#' @rdname fieldValidationAndCasting
#' @export
castCheckLabel <- function(x, coding, field_name) factor(c("", gsub(".*___(.*)", "\\1",field_name))[(x=='1'|x=='yes')+1], levels=coding, labels=names(coding))

#' @rdname fieldValidationAndCasting
#' @export
castCheckCode  <- function(x, coding, field_name) factor(c("", gsub(".*___(.*)", "\\1",field_name))[(x=='1'|x=='yes')+1], levels=coding, labels=coding)

#' @rdname fieldValidationAndCasting
#' @export

raw_cast <- list(
  date_              = NA,
  datetime_          = NA,
  datetime_seconds_  = NA,
  time_mm_ss         = NA,
  time_hh_mm_ss      = NA,
  time               = NA,
  float              = NA,
  number             = NA,
  calc               = NA,
  int                = NA,
  integer            = NA,
  yesno              = NA,
  truefalse          = NA,
  checkbox           = NA,
  form_complete      = NA,
  select             = NA,
  radio              = NA,
  dropdown           = NA,
  sql                = NA
)

#####################################################################
# Unexported - default lists for exportRecordsTyped

.default_validate <- list(
  date_              = valRx("[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])"),
  datetime_          = valRx("[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])\\s([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]"),
  datetime_seconds_  = valRx("[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])\\s([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]"),
  time_mm_ss         = valRx("[0-5][0-9]:[0-5][0-9]"),
  time_hh_mm_ss      = valRx("([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]"),
  time               = valRx("([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]"),
  float              = valRx("[-+]?(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))([Ee][+-]?[0-9]+)?"),
  number             = valRx("[-+]?(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))([Ee][+-]?[0-9]+)?"),
  calc               = valRx("[-+]?(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))([Ee][+-]?[0-9]+)?"),
  int                = valRx("^[-+]?[0-9]+(|\\.|\\.[0]+)$"),
  integer            = valRx("[-+]?[0-9]+"),
  yesno              = valRx("^(?i)(0|1|yes|no)$"),
  truefalse          = valRx("(0|1|true|false)"),
  checkbox           = valRx("^(?i)(0|1|yes|no)$"),
  form_complete      = valRx("[012]"),
  select             = valChoice,
  radio              = valChoice,
  dropdown           = valChoice,
  sql                = NA # This requires a bit more effort !?
)

.default_cast <- list(
  date_              = function(x, ...) as.POSIXct(x, format = "%Y-%m-%d"),
  datetime_          = function(x, ...) as.POSIXct(x, format = "%Y-%m-%d %H:%M"),
  datetime_seconds_  = function(x, ...) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S"),
  time_mm_ss         = function(x, ...) chron::times(ifelse(is.na(x),NA,paste0("00:",x)), format=c(times="h:m:s")),
  time_hh_mm_ss      = function(x, ...) chron::times(x, format=c(times="h:m:s")),
  time               = function(x, ...) chron::times(gsub("(^\\d{2}:\\d{2}$)", "\\1:00", x), 
                                                     format=c(times="h:m:s")),
  float              = as.numeric,
  number             = as.numeric,
  calc               = as.numeric,
  int                = as.integer,
  integer            = as.numeric,
  yesno              = castLabel,
  truefalse          = function(x, ...) x=='1' | tolower(x) =='true',
  checkbox           = castChecked,
  form_complete      = castLabel,
  select             = castLabel,
  radio              = castLabel,
  dropdown           = castLabel,
  sql                = NA
)

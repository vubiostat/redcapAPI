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
#' @param dec_symbol \code{character(1)}. The symbol in the field used to 
#'   denote a decimal. 
#' @param n_dec \code{integerish(1)}. The number of decimal places permitted
#'   by the field validation. 
#' @param checked \code{character}. Values to recognize as checked in a 
#'   checkbox field.
#' @param FUN \code{function}. A function that takes a character vector. 
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
#' \code{valChoice} constructs a validation function from a set of choices 
#' defined in the MetaData. The functions returns a TRUE/FALSE if the value
#' matches one of the choices.
#' 
#' \code{valPhone} constructs a validation function for (North American) 
#'   phone numbers. It removes punctuation and spaces prior to validating
#'   with the regular expression.
#' 
#' \code{castLabel} constructs a casting function for multiple choice variables. 
#' The field will be cast to return the choice label (generally more human readable)
#' 
#' \code{castCode} constructs a casting function for multiple choice variables.
#' Similar to \code{castLabel}, but the choice value is returned instead. The
#' values are typically more compact and their meaning may not be obvious.
#' 
#' \code{castRaw} constructs a casting function that returns the content
#' from REDCap as it was received. It is functionally equivalent to \code{identity}. 
#' For multiple choice variables, the result will be coerced to numeric, if possible; 
#' otherwise, the result is character vector.
#' 
#' \code{castChecked} constructs a casting function for checkbox fields. It
#' returns values in the form of Unchecked/Checked.
#' 
#' \code{castCheckLabel} and \code{castCheckCode} also construct casting functions
#' for checkbox fields. For both, unchecked variables are cast to an empty 
#' string (""). Checked variables are cast to the option label and option code, 
#' respectively.
#' 
#' \code{castCheckForImport} is a special case function to allow the user to
#' specify exactly which values are to be considered "Checked". Values that
#' match are returned as 1 and all other values are returned as 0. This is
#' motivated by the special case where the coding on a checkbox includes 
#' "0, Option". In the resulting field \code{checkbox___0}, a coded value
#' of 0 actually implies the choice was selected. In order to perform an 
#' import on such data, it is necessary to cast it using 
#' \code{castCheckForImport(c("0"))}.
#' 
#' \code{castDpNumeric} is a casting function for fields that use the 
#' \code{number_ndp_comma} field type (where \code{n} is the number of 
#' decimal places). This function will convert the values to numeric
#' values for use in analysis. This is a function that returns the 
#' appropriate casting function, thus the appropriate usage when using 
#' the defaults is \code{cast = list(number_1dp_comma = castDpNumeric())}
#' (using the parentheses).
#' 
#' \code{castDpCharacter} is a casting function to return fields that use
#' \code{number_ndp_comma} field types to character strings for import. This 
#' is a function that returns the appropriate casting function, thus the 
#' appropriate usage when casting for one decimal place is 
#' \code{cast = list(number_1dp_comma = castDpCharacter(1))}.
#' 
#' \code{castTimeHHMM} and \code{castTimeMMSS} are casting functions to 
#' facilitate importing data. They convert time data into a character format 
#' that will pass the API requirements. 
#' 
#' \code{raw_cast} overrides all casting if passed as the \code{cast}
#' parameter.
#' 
#' \code{na_values} A helper function to create a list of functions
#' to test for NA based on field type. Useful for bulk override of
#' NA detection for a project. The output can be directly passed to the \code{na}
#' parameter of \code{\link{exportRecordsTyped}}.
#' 
#' @author Shawn Garbett, Benjamin Nutter

#####################################################################
# Validation                                                     ####

#' @rdname fieldValidationAndCasting
#' @export
isNAorBlank <- function(x, ...) is.na(x) | x==''

#' @rdname fieldValidationAndCasting
#' @export
valRx <- function(rx) { function(x, ...) grepl(rx, x) }

#' @rdname fieldValidationAndCasting
#' @export
valChoice <- function(x, field_name, coding) x %in% coding | x %in% names(coding)

#' @rdname fieldValidationAndCasting
#' @export

valPhone <- function(x, field_name, coding){
  x <- gsub("[[:punct:][:space:]]", "", x)
  grepl(REGEX_PHONE, x)
}

#####################################################################
# Casting                                                        ####

#' @rdname fieldValidationAndCasting
#' @export
castLabel <- function(x, field_name, coding){
  code_match <- getCodingIndex(x, coding)
  
  factor(unname(coding[code_match]), levels = coding, labels = names(coding))
}

#' @rdname fieldValidationAndCasting
#' @export
castCode <- function(x, field_name, coding){
  code_match <- getCodingIndex(x, coding)
  
  factor(unname(coding[code_match]), levels = coding, labels = coding)
}

#' @rdname fieldValidationAndCasting
#' @export
castRaw <- function(x, field_name, coding){
  raw <- 
    if (grepl(".*___(.*)", field_name)){
      as.character((x %in% getCheckedValue(coding, field_name)) + 0L)
    } else {
      code_match <- getCodingIndex(x, coding)
      unname(coding[code_match])
    }
  coerceNumericIfAble(raw)
}

#' @rdname fieldValidationAndCasting
#' @export
castChecked <- function(x, field_name, coding){
  checked_value <- getCheckedValue(coding, field_name)
  
  x_checked <- x %in% checked_value 
  
  factor(c("Unchecked", "Checked")[(x_checked)+1], levels=c("Unchecked", "Checked"))
}

#' @rdname fieldValidationAndCasting
#' @export
castCheckLabel <- function(x, field_name, coding){
  checked_value <- getCheckedValue(coding, field_name)

  x_checked <- x %in% checked_value 
  
  factor(unname(c("", checked_value[1])[(x_checked) + 1]), 
         levels=c("", checked_value[1]), 
         labels=c("", names(checked_value)[1]))
}

#' @rdname fieldValidationAndCasting
#' @export
castCheckCode <- function(x, field_name, coding){
  checked_value <- getCheckedValue(coding, field_name)
  
  x_checked <- x %in% checked_value 
  
  factor(unname(c("", checked_value[1])[(x_checked) + 1]), 
         levels=c("", checked_value[1]), 
         labels=c("", checked_value[1]))
}

#' @rdname fieldValidationAndCasting
#' @export

castCheckForImport <- function(checked = c("Checked", "1")){
  function(x, coding, field_name){
    (x %in% checked) + 0L
  }
}

# utility function returns the index of the codebook matching the 
# content of the vector. Permits accurate matching without foreknowledge
# of whether the data are coded or labelled.
getCodingIndex <- function(x, coding){
  code_match <- match(as.character(x), coding)
  
  ifelse(is.na(match(x, coding)), 
         match(x, names(coding)), 
         code_match)
}

# Assembles the values that are associated with Checked
getCheckedValue <- function(coding, field_name){
  this_code <- sub(REGEX_CHECKBOX_FIELD_NAME,  # defined in constants.R
                   "\\2", field_name, perl = TRUE)
  # to match, we will convert all of the punctuation to underscore. 
  # this is consistent with how REDCap converts codes to variable names.
  # for instance, a checkbox with code 4-3, label produce variable name checkbox___4_3 
  this_code_index <- match(this_code, 
                           tolower(gsub("[[:punct:]]", "_", coding)))  

  checked_value <- c(coding[this_code_index], 
                     names(coding)[this_code_index], 
                     "1", 
                     "Checked", 
                     "yes")
  checked_value
}

# Coerce to a numeric vector if possible, otherwise return the original value
coerceNumericIfAble <- function(x){
  x <- tryCatch(as.numeric(x), 
                warning = function(cond) x, 
                error = function(cond) x)
  x
}

#####################################################################
# Casting numerics                                               ####

#' @rdname fieldValidationAndCasting
#' @export

castDpNumeric <- function(dec_symbol = ","){
  function(x, field_name, coding){
    as.numeric(sub(dec_symbol, getOption("OutDec"), x))
  }
}

#' @rdname fieldValidationAndCasting
#' @export

castDpCharacter <- function(n_dec, dec_symbol = ","){
  function(x, field_name, coding){
    x[!is.na(x)] <- format(round(as.numeric(x[!is.na(x)]), n_dec), 
                           nsmall = n_dec, 
                           decimal.mark = dec_symbol)
    x
  }
}

#' @rdname fieldValidationAndCasting
#' @export

castTimeHHMM <- function(x, field_name, coding){
  x <- as.character(x)
  x_not_missing <- x[!is.na(x)]
  time <- strsplit(x_not_missing, ":")
  time <- lapply(time, function(t) utils::head(t, 2))
  time <- vapply(time, paste0, character(1), collapse = ":")
  x[!is.na(x)] <- time
  x
}

#' @rdname fieldValidationAndCasting
#' @export

castTimeMMSS <- function(x, field_name, coding){
  x <- as.character(x)
  x_not_missing <- x[!is.na(x)]
  time <- strsplit(x_not_missing, ":")
  time <- lapply(time, function(t) utils::tail(t, 2))
  time <- vapply(time, paste0, character(1), collapse = ":")
  x[!is.na(x)] <- time
  x
}

#####################################################################
# Cast function lists                                            ####

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
  sql                = NA, 
  system             = NA
)

#####################################################################
# Unexported - default lists for exportRecordsTyped

.default_validate <- list(
  date_              = valRx(REGEX_DATE),              # REGEX values defined in constants.R
  datetime_          = valRx(REGEX_DATETIME),
  datetime_seconds_  = valRx(REGEX_DATETIME_SECONDS),
  time_mm_ss         = valRx(REGEX_TIME_MMSS),
  time_hh_mm_ss      = valRx(REGEX_TIME_HHMMSS),
  time               = valRx(REGEX_TIME),
  float              = valRx(REGEX_FLOAT),
  number             = valRx(REGEX_NUMBER),
  calc               = valRx(REGEX_CALC),
  int                = valRx(REGEX_INT),
  integer            = valRx(REGEX_INTEGER),
  yesno              = valRx(REGEX_YES_NO),
  truefalse          = valRx(REGEX_TRUE_FALSE),
  checkbox           = valRx(REGEX_CHECKBOX),
  form_complete      = valRx(REGEX_FORM_COMPLETE),
  select             = valChoice,
  radio              = valChoice,
  dropdown           = valChoice,
  sql                = NA # This requires a bit more effort !?
)

.default_cast <- list(
  date_                    = function(x, ...) as.POSIXct(x, format = "%Y-%m-%d"),
  datetime_                = function(x, ...) as.POSIXct(x, format = "%Y-%m-%d %H:%M"),
  datetime_seconds_        = function(x, ...) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S"),
  time_mm_ss               = function(x, ...) chron::times(ifelse(is.na(x),NA,paste0("00:",x)), format=c(times="h:m:s")),
  time_hh_mm_ss            = function(x, ...) chron::times(x, format=c(times="h:m:s")),
  time                     = function(x, ...) chron::times(gsub("(^\\d{2}:\\d{2}$)", "\\1:00", x), 
                                                           format=c(times="h:m:s")),
  float                    = as.numeric,
  number                   = as.numeric,
  number_1dp               = as.numeric, 
  number_1dp_comma_decimal = castDpNumeric(),
  number_2dp               = as.numeric, 
  number_2dp_comma_decimal = castDpNumeric(),
  calc                     = as.numeric,
  int                      = as.integer,
  integer                  = as.numeric,
  yesno                    = castLabel,
  truefalse                = function(x, ...) x=='1' | tolower(x) =='true',
  checkbox                 = castChecked,
  form_complete            = castLabel,
  select                   = castLabel,
  radio                    = castLabel,
  dropdown                 = castLabel,
  sql                      = NA, 
  system                   = castLabel
)

#####################################################################
# Default Lists for recastForImport                              ####

.default_validate_import <- list(
  date_                    = function(x, ...) valRx(REGEX_POSIXCT)(x) | valRx(REGEX_DATE)(x),
  datetime_                = function(x, ...) valRx(REGEX_POSIXCT)(x) | valRx(REGEX_DATETIME)(x),
  datetime_seconds_        = function(x, ...) valRx(REGEX_POSIXCT)(x) | valRx(REGEX_DATETIME_SECONDS)(x),
  time_mm_ss               = function(x, ...) valRx(REGEX_HHMMSS)(x) | valRx(REGEX_TIME_MMSS)(x),
  time_hh_mm_ss            = function(x, ...) valRx(REGEX_HHMMSS)(x) | valRx(REGEX_TIME_HHMMSS)(x),
  time                     = function(x, ...) valRx(REGEX_HHMMSS)(x) | valRx(REGEX_TIME)(x),
  alpha_only               = valRx(REGEX_LETTERS_ONLY), 
  float                    = valRx(REGEX_FLOAT),
  number                   = valRx(REGEX_NUMBER), 
  number_1dp               = valRx(REGEX_NUMBER), 
  number_1dp_comma_decimal = valRx(REGEX_NUMBER),
  number_2dp               = valRx(REGEX_NUMBER),
  number_2dp_comma_decimal = valRx(REGEX_NUMBER),
  calc                     = valRx(REGEX_CALC),
  int                      = valRx(REGEX_INT),
  integer                  = valRx(REGEX_INT),
  yesno                    = valRx(REGEX_YES_NO),
  truefalse                = valRx(REGEX_TRUE_FALSE),
  checkbox                 = valRx(REGEX_CHECKBOX),
  form_complete            = valRx(REGEX_FORM_COMPLETE),
  select                   = valChoice,
  radio                    = valChoice,
  dropdown                 = valChoice,
  email                    = valRx(REGEX_EMAIL),
  phone                    = valPhone,
  zipcode                  = valRx(REGEX_ZIPCODE),
  slider                   = valRx(REGEX_NUMBER),
  sql                      = NA # This requires a bit more effort !?
)

.default_cast_import <- list(
  date_                    = as.character,
  datetime_                = as.character,
  datetime_seconds_        = as.character,
  time_mm_ss               = castTimeMMSS,
  time_hh_mm_ss            = as.character,
  time                     = castTimeHHMM,
  alpha_only               = as.character,
  float                    = as.character,
  number                   = as.character,
  number_1dp               = castDpCharacter(1, dec_symbol = "."), 
  number_1dp_comma_decimal = castDpCharacter(1),
  number_2dp               = castDpCharacter(2, dec_symbol = "."), 
  number_2dp_comma_decimal = castDpCharacter(2),
  calc                     = as.character,
  int                      = function(x, ...) as.character(as.integer(x)),
  integer                  = function(x, ...) as.character(as.integer(x)),
  yesno                    = castRaw,
  truefalse                = function(x, ...) (x=='1' | tolower(x) =='true') + 0L,
  checkbox                 = castRaw,
  form_complete            = castRaw,
  select                   = castRaw,
  radio                    = castRaw,
  dropdown                 = castRaw,
  email                    = as.character, 
  phone                    = as.character,
  zipcode                  = as.character, 
  slider                   = as.numeric,
  sql                      = NA, 
  system                   = castRaw
)

FIELD_TYPES <- c(
  "date_",          "datetime_",  "datetime_seconds_",  "time_mm_ss",
  "time_hh_mm_ss",  "time",       "float",              "number",
  "calc",           "int",        "integer",            "yesno",
  "truefalse",      "checkbox",   "form_complete",      "select",
  "radio",          "dropdown",   "sql")

#' @rdname fieldValidationAndCasting
#' @export
na_values <- function(FUN)
{
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_function( x       = FUN,
                              null.ok = FALSE,
                              add     = coll)
  checkmate::reportAssertions(coll)
  
  l <- lapply(FIELD_TYPES, function(f) FUN)
  names(l) <- FIELD_TYPES
  l
}


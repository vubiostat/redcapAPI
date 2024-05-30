#' @name fieldValidationAndCasting
#' @title Helper functions for `exportRecordsTyped` Validation and Casting
#' @description This set of functions assists in validating that the content of 
#'   fields coming from REDCap match the MetaData, allowing for a 
#'   validation report to provided. The cast helpers allow for transforming
#'   the REDCap data into R data types and allowing the user to customize 
#'   the end product.
#'   
#' @param x `character`. A vector to check.
#' @param rx `character`. The regular expression pattern to check.
#' @param coding named `character` vector. The defined coding from the meta data.
#' @param field_name `character(1)`. Name of the field(s)
#' @param dec_symbol `character(1)`. The symbol in the field used to 
#'   denote a decimal. 
#' @param n_dec `integerish(1)`. The number of decimal places permitted
#'   by the field validation. 
#' @param checked `character`. Values to recognize as checked in a 
#'   checkbox field.
#' @param FUN `function`. A function that takes a character vector. 
#' @param ... Consumes anything else passed to function. I.e., field_name and 
#' coding.
#'
#' @details Functions passed to the `na`, `validation`, and
#' `cast` parameter of [exportRecordsTyped()] all take the form
#' of `function(x, coding, field_name)`. `na` and `validation`
#' functions are expected to return a logical vector of the same length as the
#' column processed. Helper routines
#' are provided here for common cases to construct these functions.
#' 
#' ## Missing Data Detection
#' 
#' `na_values` is a helper function to create a list of functions
#' to test for NA based on field type. Useful for bulk override of
#' NA detection for a project. The output can be directly passed to the `na`
#' parameter of [exportRecordsTyped()].
#' 
#' Missing data detection is performed ahead of validation. Data that are found
#' to be missing are excluded from validation reports. 
#' 
#' REDCap users may define project-level missing value codes. If such codes
#' are defined, they can be seen in Project Setup > Additional Customizations >
#' Missing Data Codes. They will also be displayed in the project's Codebook. 
#' Project-level missing data codes cannot be accessed via the API, meaning
#' `redcapAPI` is unable to assist in determining if a project has any. The 
#' most likely symptom of project-level codes is a high frequency of 
#' values failing validation (See `vignette("redcapAPI-missing-data-detection")`).
#' 
#' ## Validation Functions
#' 
#' `isNAorBlank` returns TRUE/FALSE if field is NA or blank. Helper
#' function for constructing `na` overrides in [exportRecordsTyped()].
#' 
#' `valRx` constructs a validation function from a regular expression pattern. 
#' The function returns a TRUE/FALSE if the value matches the pattern.
#' 
#' `valChoice` constructs a validation function from a set of choices 
#' defined in the MetaData. The functions returns a TRUE/FALSE if the value
#' matches one of the choices.
#' 
#' `valPhone` constructs a validation function for (North American) 
#'   phone numbers. It removes punctuation and spaces prior to validating
#'   with the regular expression.
#'   
#' `valSkip` is a function that supports skipping the validation for 
#'   a field type. It returns a `TRUE` value for each record, regardless
#'   of its value. Validation skipping has occasional utility when importing
#'   certain field types (such as `bioportal` or `sql`) where not all of the
#'   eventual choices are available in the project yet.
#'   
#' `skip_validation` is a list of functions that just returns TRUE for
#'   all data passed in.
#' 
#' ## Casting Functions
#' 
#' `castLabel` constructs a casting function for multiple choice variables. 
#' The field will be cast to return the choice label (generally more human readable). 
#' `castLabelCharacter` is an equivalent casting function that returns 
#' a `character` vector instead of a `factor`.
#' 
#' `castCode` constructs a casting function for multiple choice variables.
#' Similar to `castLabel`, but the choice value is returned instead. The
#' values are typically more compact and their meaning may not be obvious.
#' `castCodeCharacter` is an equivalent casting function that retuns
#' a `character` vector instead of a `factor`.
#' 
#' `castRaw` constructs a casting function that returns the content
#' from REDCap as it was received. It is functionally equivalent to `identity`. 
#' For multiple choice variables, the result will be coerced to numeric, if possible; 
#' otherwise, the result is character vector. 
#' 
#' `castChecked` constructs a casting function for checkbox fields. It
#' returns values in the form of Unchecked/Checked. `castCheckedCharacter`
#' is an equivalent casting function that returns a `character` vector 
#' instead of a `factor`.
#' 
#' `castCheckLabel` and `castCheckCode` also construct casting functions
#' for checkbox fields. For both, unchecked variables are cast to an empty 
#' string (""). Checked variables are cast to the option label and option code, 
#' respectively. `castCheckLabelCharacter` and `castCheckCodeCharacter`
#' are equivalent casting functions that returns a `character` vector 
#' instead of a `factor`.
#' 
#' `castCheckForImport` is a special case function to allow the user to
#' specify exactly which values are to be considered "Checked". Values that
#' match are returned as `1` and all other values are returned as `0`. This is
#' motivated by the special case where the coding on a checkbox includes 
#' "0, Option". In the resulting field `checkbox___0`, a coded value
#' of `0` actually implies the choice was selected. In order to perform an 
#' import on such data, it is necessary to cast it using 
#' `castCheckForImport(c("0"))`.
#' 
#' `castDpNumeric` is a casting function for fields that use the 
#' `number_ndp_comma` field type (where `n` is the number of 
#' decimal places). This function will convert the values to numeric
#' values for use in analysis. This is a function that returns the 
#' appropriate casting function, thus the appropriate usage when using 
#' the defaults is `cast = list(number_1dp_comma = castDpNumeric())`
#' (using the parentheses).
#' 
#' `castDpCharacter` is a casting function to return fields that use
#' `number_ndp_comma` field types to character strings for import. This 
#' is a function that returns the appropriate casting function, thus the 
#' appropriate usage when casting for one decimal place is 
#' `cast = list(number_1dp_comma = castDpCharacter(1))`.
#' 
#' `castTimeHHMM` and `castTimeMMSS` are casting functions to 
#' facilitate importing data. They convert time data into a character format 
#' that will pass the API requirements. 
#' 
#' `castLogical` is a casting function that returns a logical vector for 
#' common, binary-type responses. It is well suited to changing true/false, 
#' yes/no, and checkbox fields into logical vectors, as it returns `TRUE` if
#' the value is one of `c("1", "true", "yes")` and returns `FALSE` otherwise.
#' 
#' 
#' ## Casting Lists
#' `raw_cast` overrides all casting if passed as the `cast`
#' parameter. It is important the the validation specified matches
#' the chosen cast. For fully raw it should be `skip_validation`.
#' 
#' `default_cast_no_factor` is a list of casting functions that matches
#' all of the default casts but with the exception that any fields that would
#' have been cast to factors will instead be cast to characters. It is 
#' provided for the user that prefers to work absent factors. The list
#' `default_cast_character` is equivalent and is provided for those that
#' prefer to describe their casting in terms of what the result is (and not
#' what it is not).
#' 
#' 
#' @return 
#' 
#' Validation and casting functions return the objects indicated in the 
#' following table:
#' 
#' | Function Name             | Object Type Returned |
#' |---------------------------|----------------------| 
#' | `isNAOrBlank`             | `logical`            |
#' | `valRx`                   | `logical`            |
#' | `valChoice`               | `logical`            |
#' | `valPhone`                | `logical`            |
#' | `valSkip`                 | `logical`            |
#' | `castLabel`               | `factor`             |
#' | `castLabelCharacter`      | `character`          |
#' | `castCode`                | `factor`             |
#' | `castCodeCharacter`       | `character`          |
#' | `castRaw`                 | `character`          |
#' | `castChecked`             | `factor`             |
#' | `castCheckedCharacter`    | `character`          |
#' | `castCheckLabel`          | `factor`             |
#' | `castCheckLabelCharacter` | `character`          |
#' | `castCheckCode`           | `factor`             |
#' | `castCheckCodeCharacter`  | `character`          | 
#' | `castCheckForImport`      | `numeric`            |
#' | `castDpNumeric`           | `numeric`            |
#' | `castDpCharacter`         | `character`          |
#' | `castTimeHHMM`            | `character`          |
#' | `castTimeMMSS`            | `character`          |
#' | `castLogical`             | `logical`            |
#' 
#' 
#' @seealso 
#' [fieldCastingFunctions()], \cr
#' [exportRecordsTyped()], \cr
#' [exportReportsTyped()]
#' 
#' ## Vignettes
#' 
#' `vignette("redcapAPI-casting-data")`\cr
#' `vignette("redcapAPI-missing-data-detection")`\cr
#' `vignette("redcapAPI-data-validation)`\cr
#' `vignette("redcapAPI-faq)`
#' 
#' @examples
#' \dontrun{
#' # Make a custom function to give special treatment to a field.
#' # In this function, the field "field_name_to_skip" will 
#' # be cast using `castRaw`. All other fields will be cast 
#' # using `castCode`
#' customCastCode <- function(x, field_name, coding){
#'   if (field_name == "field_name_to_skip"){
#'     castRaw(x, field_name, coding)
#'   } else {
#'     castCode(x, field_name, coding)
#'   }
#' }
#' }
#' 

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

#' @rdname fieldValidationAndCasting
#' @export

valSkip <- function(x, field_name, coding){
  rep(TRUE, length(x))
}

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
castLabelCharacter <- function(x, field_name, coding){
  as.character(castLabel(x, field_name, coding))
}

#' @rdname fieldValidationAndCasting
#' @export
castCode <- function(x, field_name, coding){
  code_match <- getCodingIndex(x, coding)
  
  factor(unname(coding[code_match]), levels = coding, labels = coding)
}

#' @rdname fieldValidationAndCasting
#' @export
castCodeCharacter <- function(x, field_name, coding){
  as.character(castCode(x, field_name, coding))
}

#' @rdname fieldValidationAndCasting
#' @export
castRaw <- function(x, field_name, coding){
  warnOfZeroCodedCheckCasting(field_name, x)
  
  raw <- 
    if (grepl(".*___(.*)", field_name)){
      ifelse(!is.na(x), 
             as.character((x %in% getCheckedValue(coding, field_name)) + 0L), 
             NA)
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
  
  warnOfZeroCodedCheckCasting(field_name, x)
  
  x_checked <- x %in% checked_value 
  
  factor(c("Unchecked", "Checked")[(x_checked)+1], levels=c("Unchecked", "Checked"))
}

#' @rdname fieldValidationAndCasting
#' @export
castCheckedCharacter <- function(x, field_name, coding){
  as.character(castChecked(x, field_name, coding))
}

#' @rdname fieldValidationAndCasting
#' @export
castCheckLabel <- function(x, field_name, coding){
  checked_value <- getCheckedValue(coding, field_name)
  
  warnOfZeroCodedCheckCasting(field_name, x)
  
  x_checked <- x %in% checked_value 
  
  # Sets the level and label while accomodating 0 coded check values
  # (0 is not considered a `checked_value` in this case, so must be handled by force)
  
  is_zero_coded <- isZeroCodedCheckField(field_name)
  
  the_level <- if (is_zero_coded) "0"              else checked_value[1]
  the_label <- if (is_zero_coded) checked_value[1] else names(checked_value)[1]
  
  factor(unname(c("", the_level)[(x_checked) + 1]), 
         levels=c("", the_level), 
         labels=c("", the_label))
}

#' @rdname fieldValidationAndCasting
#' @export
castCheckLabelCharacter <- function(x, field_name, coding){
  as.character(castCheckLabel(x, field_name, coding))
}

#' @rdname fieldValidationAndCasting
#' @export
castCheckCode <- function(x, field_name, coding){
  checked_value <- getCheckedValue(coding, field_name)
  
  warnOfZeroCodedCheckCasting(field_name, x)
  
  x_checked <- x %in% checked_value 
  
  # Sets the level and label while accomodating 0 coded check values
  # (0 is not considered a `checked_value` in this case, so must be handled by force)
  
  is_zero_coded <- isZeroCodedCheckField(field_name)
  
  the_level <- if (is_zero_coded) "0" else checked_value[1]
  the_label <- if (is_zero_coded) "0" else checked_value[1]
  
  factor(unname(c("", the_level)[(x_checked) + 1]), 
         levels=c("", the_level), 
         labels=c("", the_label))
}

#' @rdname fieldValidationAndCasting
#' @export
castCheckCodeCharacter <- function(x, field_name, coding){
  as.character(castCheckCode(x, field_name, coding))
}

#' @rdname fieldValidationAndCasting
#' @export

castCheckForImport <- function(checked = c("Checked", "1")){
  function(x, coding, field_name){
    is_na <- is.na(x)
    
    out <- (x %in% checked) + 0L
    out[is_na] <- NA
    out
  }
}

# utility function returns the index of the codebook matching the 
# content of the vector. Permits accurate matching without foreknowledge
# of whether the data are coded or labeled.
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
  
  # When casting from raw, we do not want to consider "0" as checked 
  # for zero coded fields.
  if (isZeroCodedCheckField(field_name)){
    checked_value <- checked_value[-1]
  } 
  
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

#' @rdname fieldValidationAndCasting
#' @export

castLogical <- function(x, field_name, coding){
  is_na <- is.na(x)
  x <- x %in% c("1", "true", "yes")
  x[is_na] <- NA
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
  system             = NA, 
  bioportal          = NA
)

#' @rdname fieldValidationAndCasting
#' @export

default_cast_no_factor <- list(
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
  yesno                    = castLabelCharacter,
  truefalse                = function(x, ...) x=='1' | tolower(x) =='true',
  checkbox                 = castCheckedCharacter,
  form_complete            = castLabelCharacter,
  select                   = castLabelCharacter,
  radio                    = castLabelCharacter,
  dropdown                 = castLabelCharacter,
  sql                      = castLabelCharacter, 
  system                   = castLabelCharacter, 
  bioportal                = castLabelCharacter
)

#' @rdname fieldValidationAndCasting
#' @export

default_cast_character <- default_cast_no_factor


#####################################################################
# Unexported - default lists for exportRecordsTyped              ####

.default_validate <- list(
  # REGEX values defined in constants.R
  date_              = function(x, ...) !is.na(as.POSIXct(x, format = "%Y-%m-%d")),              
  datetime_          = function(x, ...) !is.na(as.POSIXct(x, format = "%Y-%m-%d %H:%M")), 
  datetime_seconds_  = function(x, ...) !is.na(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S")),
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
  sql                = valChoice, 
  bioportal          = valSkip
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
  sql                      = castLabel, 
  system                   = castLabel, 
  bioportal                = castLabel
)

#####################################################################
# Default Lists for recastForImport                              ####

.default_validate_import <- list(
  date_                    = function(x, ...) !is.na(as.POSIXct(x, format = "%Y-%m-%d")),
  datetime_                = function(x, ...) !is.na(as.POSIXct(x, format = "%Y-%m-%d %H:%M")),
  datetime_seconds_        = function(x, ...) !is.na(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S")),
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
  sql                      = valChoice,
  bioportal                = valChoice
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
  sql                      = castRaw, 
  system                   = castRaw, 
  bioportal                = castCodeCharacter
)

#####################################################################
# FIELD_TYPES constant                                           ####

FIELD_TYPES <- c(
  "date_",          "datetime_",  "datetime_seconds_",  "time_mm_ss",
  "time_hh_mm_ss",  "time",       "float",              "number",
  "calc",           "int",        "integer",            "yesno",
  "truefalse",      "checkbox",   "form_complete",      "select",
  "radio",          "dropdown",   "sql",                "system", 
  "bioportal")



  #####################################################################
 #
# Validation lists 

#' @rdname fieldValidationAndCasting
#' @export
skip_validation <- na_values(valSkip)


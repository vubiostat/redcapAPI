# validate_import_form_complete -------------------------------------
# Tests to perform 
# * The following values are mapped
#     Incomplete to 0
#     Unverified to 1
#     Complete to 2
# * 0, 1, and 2 are returned unchanged
# * NA values are silently ignored (no output)
# * any other value produces a validation message

validate_import_form_complete <- function(x, field_name, logfile)
{
  x <- as.character(x)
  x <- gsub(pattern = "Incomplete", 
            replacement = "0", 
            x = x)
  x <- gsub(pattern = "Unverified", 
            replacement = "1", 
            x = x)
  x <- gsub(pattern = "Complete", 
            replacement = "2", 
            x = x)
  x <- trimws(x)
  
  w <- which((!x %in% 0:2) & !is.na(x))
  x[w] <- NA
  
  print_validation_message(
    field_name, 
    indices = w,
    message = paste0("Values(s) must be one of: 0, 1, 2, ",
                     "Incomplete, Unverified, or Complete.\n", 
                     "Value not imported"),
    logfile = logfile)
  
  x
}

# validate_import_date ----------------------------------------------
# * Date and POSIXct values are returned in YYYY-mm-dd format
# * map ymd, ymd HMS, mdy, mdy HMS, dmy, and dmy HMS strings to YYYY-mm-dd format
# * NA values pass silently
# * Unmappable values return a message
# * When a date is less than field_min, a message is returned.
# * When a date is greater than field_max, a message is returned.

validate_import_date <- function(x, field_name, field_min, field_max, logfile)
{
  if (isTRUE(trimws(field_min) == "today")) field_min = format(Sys.Date())
  if (isTRUE(trimws(field_min) == "now")) field_min = format(Sys.time())
  
  if (isTRUE(trimws(field_max) == "today")) field_max = format(Sys.Date())
  if (isTRUE(trimws(field_max) == "now")) field_max = format(Sys.time())
  
  x_orig <- x
  if (!inherits(x, "Date") && !inherits(x, "POSIXct"))
  {
    suppressWarnings(
      x <- lubridate::parse_date_time(x = x,
                                      orders = c("ymd", "ymd HMS",
                                                 "mdy", "mdy HMS",
                                                 "dmy", "dmy HMS"))
    )
  }
  
  w_low <- which(as.POSIXct(x) < as.POSIXct(field_min, origin = "1970-01-01"))
  print_validation_message(
    field_name,
    indices = w_low,
    message = paste0("Value(s) are before the stated minimum date: ",
                     format(field_min, format = "%Y-%m-%d")),
    logfile = logfile
  )
  
  w_high <- which(as.POSIXct(x) > as.POSIXct(field_max, origin = "1970-01-01"))
  print_validation_message(
    field_name,
    indices = w_high,
    message = paste0("Values(s) are after the stated maximum date: ",
                     format(field_max, format = "%Y-%m-%d")),
    logfile = logfile
  )
  
  
  x <- format(x, format = "%Y-%m-%d")
  
  w <- which(is.na(x) & !x_orig %in% c('', NA))
  
  print_validation_message(
    field_name, 
    indices = w,
    message = paste0("Value(s) must have POSIXct class, Date class, ",
                     "or character class in ymd, mdy, or dmy format (with optional HMS).\n", 
                     "Value not imported"),
    logfile = logfile)
  
  x
}

# validate_import_datetime ------------------------------------------
# * Date and POSIXct values are returned in YYYY-mm-dd HH:MM format
# * map ymd, ymd HMS, mdy, mdy HMS, dmy, and dmy HMS strings to YYYY-mm-dd HH:MM format
# * NA values pass silently
# * Unmappable values return a message
# * When a date is less than field_min, a message is returned.
# * When a date is greater than field_max, a message is returned.
validate_import_datetime <- function(x, field_name, field_min, field_max, logfile)
{
  if (isTRUE(trimws(field_min) == "today")) field_min = format(Sys.Date())
  if (isTRUE(trimws(field_min) == "now")) field_min = format(Sys.time())
  
  if (isTRUE(trimws(field_max) == "today")) field_max = format(Sys.Date())
  if (isTRUE(trimws(field_max) == "now")) field_max = format(Sys.time())
  
  x_orig <- x
  if (!inherits(x, "Date") && !inherits(x, "POSIXct"))
  {
    suppressWarnings(
      x <- lubridate::parse_date_time(x = x,
                                      orders = c("ymd", "ymd HMS", "ymd HM", 
                                                 "mdy", "mdy HMS", "mdy HM",
                                                 "dmy", "dmy HMS", "dmy HM"))
    )
  }
  
  w_low <- which(as.POSIXct(x) < as.POSIXct(field_min, origin = "1970-01-01"))
  print_validation_message(
    field_name,
    indices = w_low,
    message = paste0("Value(s) are before the stated minimum date: ",
                     format(field_min, format = "%Y-%m-%d %H:%M")),
    logfile = logfile
  )
  
  w_high <- which(as.POSIXct(x) > as.POSIXct(field_max, origin = "1970-01-01"))
  print_validation_message(
    field_name,
    indices = w_high,
    message = paste0("Values(s) are after the stated maximum date: ",
                     format(field_max, format = "%Y-%m-%d %H:%M")),
    logfile = logfile
  )
  
  x <- format(x, format = "%Y-%m-%d %H:%M")
  
  w <- which(is.na(x) & !x_orig %in% c('', NA))
  
  print_validation_message(
    field_name, 
    indices = w,
    message = paste0("Value(s) must have POSIXct class, Date class, ",
                     "or character class in ymd, mdy, or dmy format (with optional HMS).\n", 
                     "Value not imported"),
    logfile = logfile)
  
  x
}

# validate_import_datetime_seconds ----------------------------------
# * Date and POSIXct values are returned in YYYY-mm-dd HH:MM format
# * map ymd, ymd HMS, mdy, mdy HMS, dmy, and dmy HMS strings to YYYY-mm-dd HH:MM format
# * NA values pass silently
# * Unmappable values return a message
# * When a date is less than field_min, a message is returned.
# * When a date is greater than field_max, a message is returned.
validate_import_datetime_seconds <- function(x, field_name, field_min, field_max, logfile)
{
  if (isTRUE(trimws(field_min) == "today")) field_min = format(Sys.Date())
  if (isTRUE(trimws(field_min) == "now")) field_min = format(Sys.time())
  
  if (isTRUE(trimws(field_max) == "today")) field_max = format(Sys.Date())
  if (isTRUE(trimws(field_max) == "now")) field_max = format(Sys.time())
  
  x_orig <- x
  if (!inherits(x, "Date") && !inherits(x, "POSIXct"))
  {
    suppressWarnings(
      x <- lubridate::parse_date_time(x = x,
                                      orders = c("ymd", "ymd HMS",
                                                 "mdy", "mdy HMS",
                                                 "dmy", "dmy HMS"))
    )
  }
  
  w_low <- which(as.POSIXct(x) < as.POSIXct(field_min))
  print_validation_message(
    field_name,
    indices = w_low,
    message = paste0("Value(s) are before the stated minimum date: ",
                     format(field_min, format = "%Y-%m-%d %H:%M:%S")),
    logfile = logfile
  )
  
  w_high <- which(as.POSIXct(x) > as.POSIXct(field_max))
  print_validation_message(
    field_name,
    indices = w_high,
    message = paste0("Values(s) are after the stated maximum date: ",
                     format(field_max, format = "%Y-%m-%d %H:%M:%S")),
    logfile = logfile
  )
  
  x <- format(x, format = "%Y-%m-%d %H:%M:%S")
  
  w <- which(is.na(x) & !x_orig %in% c('', NA))
  
  print_validation_message(
    field_name, 
    indices = w,
    message = paste0("Value(s) must have POSIXct class, Date class, ",
                     "or character class in ymd, mdy, or dmy format (with optional HMS).\n", 
                     "Value not imported"),
    logfile = logfile)
  
  x
}

# validate_import_time ----------------------------------------------
# Tests to perform
# * character forms of HH:MM and HH:MM:SS pass
# * objects of class time pass
# * NA passes
# * times before field_min produce a message
# * times after field_max produce a message
validate_import_time <- function(x, field_name, field_min, field_max, logfile)
{
  x <- as.character(x)
  
  w_invalid <- !grepl("^(\\d{2}:\\d{2}:00|\\d{2}:\\d{2})$", x)
  x[w_invalid] <- NA
  
  count_minute <- function(t)
  {
    if (is.na(t)) return(NA_real_)
    t <- strsplit(t, ":")
    t <- unlist(t)
    t <- as.numeric(t)
    t[1] * 60 + t[2]
  }

  total_min <- vapply(x, count_minute, numeric(1))
  
  print_validation_message(
    field_name,
    indices = which(total_min < count_minute(field_min)),
    message = paste0("Value(s) are before the stated minimum time: ",
                     field_min),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(total_min > count_minute(field_max)),
    message = paste0("Value(s) are after the stated maximum time: ",
                     field_max),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(w_invalid),
    message = paste0("Value(s) must be of class `time` or in one of hh:mm:ss or hh:mm formats.\n",
                     "Values not imported",
                     field_min),
    logfile = logfile
  )
  
  substr(x, 1, 5)
}

# validate_import_time_mm_ss ----------------------------------------
# Tests to perform
# * character forms of HH:MM and HH:MM:SS pass
# * objects of class time pass
# * NA passes
# * times before field_min produce a message
# * times after field_max produce a message
validate_import_time_mm_ss <- function(x, field_name, field_min, field_max, logfile)
{
  x <- as.character(x)
  
  x[grepl("^\\d{2}:\\d{2}:\\d{2}$", x)] <- 
    sub("^\\d{2}:", "", x[grepl("^\\d{2}:\\d{2}:\\d{2}$", x)])
  
  w_invalid <- !grepl("^\\d{2}:\\d{2}$", x) & !is.na(x)
  x[w_invalid] <- NA
  
  count_second <- function(t)
  {
    if (is.na(t)) return(NA)
    t <- strsplit(t, ":")
    t <- unlist(t)
    t <- as.numeric(t)
    t[1] * 60 + t[2]
  }
  
  total_sec <- vapply(x, count_second, numeric(1))
  
  print_validation_message(
    field_name,
    indices = which(total_sec < count_second(field_min)),
    message = paste0("Value(s) are before the stated minimum time: ",
                     field_min),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(total_sec > count_second(field_max)),
    message = paste0("Value(s) are after the stated maximum time: ",
                     field_max),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(w_invalid),
    message = paste0("Value(s) must be of class `time` or in one of hh:mm:ss or hh:mm formats.\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_numeric -------------------------------------------
# Tests to perform
# * values that can be coerced to numeric pass.
# * NA passes
# * Values that cannot be coerced to numeric produce a message
# * values less than field_min produce a message
# * values greater than field_max produce a message
validate_import_numeric <- function(x, field_name, field_min, field_max, logfile)
{
  suppressWarnings(num_check <- as.numeric(x))
  w <- which(is.na(num_check) & !x %in% c('', NA))
  
  suppressWarnings({
    if (!is.numeric(x)) x <- as.numeric(x)
    field_min <- as.numeric(field_min)
    field_max <- as.numeric(field_max)
  })
  
  print_validation_message(
    field_name,
    indices = which(x < field_min),
    message = paste0("Value(s) are less than the stated minimum: ",
                     field_min),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(x > field_max),
    message = paste0("Value(s) are greater than the stated maximum: ",
                     field_max),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be numeric or coercible to numeric.\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_number_dp -------------------------------------------
# Tests to perform
# * values that can be coerced to numeric pass.
# * NA passes
# * Values that cannot be coerced to numeric produce a message
# * values less than field_min produce a message
# * values greater than field_max produce a message
validate_import_ndp <- function(x, field_name, field_min, field_max, logfile, 
                                    ndp, comma = FALSE)
{
  suppressWarnings(num_check <- as.numeric(x))
  w <- which(is.na(num_check) & !x %in% c('', NA))
  
  suppressWarnings({
    if (!is.numeric(x)) x <- as.numeric(x)
    field_min <- as.numeric(field_min)
    field_max <- as.numeric(field_max)
  })
  
  print_validation_message(
    field_name,
    indices = which(x < field_min),
    message = paste0("Value(s) are less than the stated minimum: ",
                     field_min),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(x > field_max),
    message = paste0("Value(s) are greater than the stated maximum: ",
                     field_max),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be numeric or coercible to numeric.\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x[!is.na(x)] <- trimws(format(round(x[!is.na(x)], ndp), 
                                nsmall = ndp, 
                                decimal.mark = if (comma) "," else "."))
  
  x
}

# validate_import_zipcode -------------------------------------------
# Tests to run
# * values in 12345 and 12345-1234 format pass
# * NA values pass
# * unacceptable values produce a message
validate_import_zipcode <- function(x, field_name, logfile)
{
  x <- as.character(x)
  x <- trimws(x)
  w <- which(!grepl("^(\\d{5}|\\d{5}-\\d{4})$", x) & !is.na(x))
  
  x[w] <- rep(NA_character_, length(w))
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be in the format `12345` or `12345-1234`.\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_yesno ---------------------------------------------
# Test to run
# * yes, no, 0, and 1 are accepted
# * NA is accepted
# * other numeric values produce a message
# * other character values produce a message

validate_import_yesno <- function(x, field_name, logfile)
{
  x <- as.character(x)
  x <- tolower(x)
  w <- which(!x %in% c("no", "yes", "0", "1") & !is.na(x))
  x[w] <- rep(NA_character_, length(w))
  
  x <- gsub("no", "0", x)
  x <- gsub("yes", "1", x)
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be one of `0`, `1`, `No`, or `Yes` (Ignoring case).\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_truefalse -----------------------------------------
# Test to run
# * true, false, yes, no, 0, and 1 are accepted
# * NA is accepted
# * other numeric values produce a message
# * other character values produce a message

validate_import_truefalse <- function(x, field_name, logfile)
{
  x <- as.character(x)
  x <- tolower(x)
  w <- which(!x %in% c("true", "false", "0", "1", "no", "yes") & !is.na(x))
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be one of logical or one of `0`, `1`, `No`, `Yes`, `False`, or `True` (Ignoring case).\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x[w] <- NA
  
  x <- gsub("(true|yes)", 1, x)
  x <- gsub("(false|no)", 0, x)
  x
}

# validate_import_select_dropdown_radio -----------------------------
# Tests to run
# mapped pairings with numeric and character codes pass
# NA passes
# unmapped values are converted to NA
# unmapped values produce a message
validate_import_select_dropdown_radio <- function(x, field_name, field_choice, logfile)
{
  x <- as.character(x)
  mapping <- fieldChoiceMapping(field_choice, field_name)  
  
  #* Return labeled values to coded values
  for (i in seq_len(nrow(mapping))){
    x[x == mapping[i, 2]] <- mapping[i, 1]  
  }
  
  w <- which(!x %in% mapping[, 1] & !x %in% c('', NA))
  x[w] <- rep(NA_character_, length(w))
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be one of '",
                     paste0(mapping[, 1], collapse = "', '"), "', '",
                     paste0(mapping[, 2], collapse = "', '"),
                     "'.\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_checkbox ------------------------------------------
# Tests to run
# * 0, 1, Unchecked, Checked, '', NA all pass.
# * codes and labels pass
# * Other values produce a message

validate_import_checkbox <- function(x, field_name, field_choice, logfile)
{
  x <- trimws(tolower(as.character(x)))
  
  checkChoice <- fieldChoiceMapping(field_choice, field_name)
  
  #* Select the labeled string from the options as a valid input for the import.
  checkChoice <- checkChoice[checkChoice[, 1] == unlist(strsplit(field_name, "___"))[2], ]
  
  w <- which(!x %in% c("checked", "unchecked", "0", "1", tolower(checkChoice), "", NA))

  x <- gsub("^checked$", "1", x)
  x <- gsub("^unchecked$", "0", x)
  x[!x %in% c("0", "1") & x %in% tolower(checkChoice)] <- 1

  x[x == ""] <- 0
  x[!x %in% c("0", "1")] <- NA
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be one of '0', '1', 'Checked', 'Unchecked', '",
                     paste0(checkChoice, collapse = "', '"), 
                     "', '' (ignoring case).\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_email ---------------------------------------------
# Tests to run
# * Common email addresses pass
# * Invalid e-mail addresses are changed to NA 
#     - have more than one @
#     - have no @
#     - have no suffix
#     - have a suffix of length one
#     - have a suffix exceeding length 6
# * Invalid e-mail addresses produce a message

validate_import_email <- function(x, field_name, logfile)
{
  x <- as.character(x)
  w <- which((!grepl("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+[.][A-Za-z]{2,6}$", x) |
                grepl("[@].+[@]", x)) & !is.na(x))
  
  print_validation_message(
    field_name = field_name,
    indices = w,
    message = paste0("Value(s) are not valid e-mail addresses.\n",
                     "Values not imported."),
    logfile = logfile
  )
  
  x[w] <- NA
  
  x
}

# validate_import_phone ---------------------------------------------
# Tests to perform
# * valid phone numbers pass
# * NA passes
# * phone numbers of more than ten digits become NA
# * phone numbers of more than ten digits produce a message
# * phone numbers with invalid format become NA
# * phone numbers with invalid format produce a message

validate_import_phone <- function(x, field_name, logfile)
{
  x <- as.character(x)
  x <- gsub("[[:punct:][:space:]]", "", x)
  
  w_long <- nchar(x) != 10 & !is.na(x)
  
  w_invalid <- !grepl("^[2-9][0-8][0-9][2-9][0-9]{6}$", x) & !is.na(x)
  
  print_validation_message(
    field_name = field_name,
    indices = which(w_long),
    message = paste0("Value(s) are not ten digit phone numbers.\n",
                     "Values not imported."), 
    logfile = logfile
  )
  
  print_validation_message(
    field_name = field_name,
    indices = which(w_invalid),
    message = paste0("Value(s) are not valid North American phone numbers.\n",
                     "Values not imported."), 
    logfile = logfile
  )
  
  x[w_long | w_invalid] <- NA_character_
  x
}

# print_validation_message ------------------------------------------

print_validation_message <- function(field_name, indices, message, logfile)
{
  if (length(indices))
  {
    message <- 
      paste0("------------------------------------\n",
             "Field Name: `", field_name, "`\n",
             "Indices: ", paste0(indices, collapse = ", "), "\n",
             message, "\n\n")
    
    if (logfile == "")
    {
      message(message)
    }
    else  
    {
      write(message, 
            file = logfile,
            append = TRUE)
    }
  }
}

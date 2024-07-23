# NOTES TO DEVELOPERS
# The documentation elements in this file are those that are commonly used
# across multiple functions. They are deliberately set up in a way that
# will prevent them from having a .Rd file written AND from being included
# in the package help index. 
#
# This also means devtools::document() will produce a warning
# "File lacks name and/or title"
# This warning should be ignored for these entries. Do not try to 
# add a name or title these documentations.

#' @name common-rcon-arg
#' @keywords internal
#' @description Common redcapConnection documentation (for rcon)
#' 
#' @param rcon A `redcapConnection` object.

NULL

#' @name common-api-args
#' @keywords internal
#' @description Common API arguments
#' 
#' @param config A named `list`. Additional configuration parameters to pass to
#'   [curl::handle_setopt]. These are appended to any parameters in
#'   `rcon$config`.
#' @param api_param A named `list`. Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by `redcapAPI`.

NULL

#' @name common-cast-args
#' @keywords internal
#' @description Common casting arguments used in documenation
#' 
#' @param na  A named `list` of user specified functions to determine if the
#'   data is NA. This is useful when data is loaded that has coding for NA, e.g.
#'   -5 is NA. Keys must correspond to a truncated REDCap field type, i.e.
#'   date_, datetime_, datetime_seconds_, time_mm_ss, time_hh_mm_ss, time, float,
#'   number, calc, int, integer, select, radio, dropdown, yesno, truefalse,
#'   checkbox, form_complete, sql, system. The function will be provided the 
#'   variables (x, field_name, coding). The function must return a vector of 
#'   logicals matching the input. It defaults to [isNAorBlank()] for all
#'   entries.
#' @param validation A named `list` of user specified validation functions. The 
#'   same named keys are supported as the na argument. The function will be 
#'   provided the variables (x, field_name, coding). The function must return a
#'   vector of logical matching the input length. Helper functions to construct
#'   these are [valRx()] and [valChoice()]. Only fields that
#'   are not identified as NA will be passed to validation functions. 
#' @param cast A named `list` of user specified class casting functions. The
#'   same named keys are supported as the na argument. The function will be 
#'   provided the variables (x, field_name, coding). The function must return a
#'   vector of logical matching the input length. The cast should match the validation,
#'   if one is using `raw_cast`, then `validation=skip_validation` is likely
#'   the desired intent. See [fieldValidationAndCasting()]
#' @param assignment A named `list` of functions. These functions are provided, field_name,
#'   label, description and field_type and return a list of attributes to assign
#'   to the column. Defaults to creating a label attribute from the stripped
#'   HTML and UNICODE raw label and scanning for units=\{"UNITS"\} in description

NULL

#' @name common-dot-args
#' @keywords internal
#' 
#' @description Common API arguments
#' 
#' @param ... Arguments to pass to other methods

NULL

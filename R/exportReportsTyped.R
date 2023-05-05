#' @name exportReportsTyped
#' @title Export a Report from a REDCap Project
#' 
#' @details This method allows you to export the data set of a report 
#'   created on a project's 'Data Exports, Reports, and Stats' page.
#'   
#' @param rcon A \code{redcapConnection} object
#' @param report_id \code{integerish(1)} The ID number of the report to 
#'   be exported. 
#' @param drop_fields \code{character}. A vector of field names to remove 
#'   from the export. Ignore if length = 0.
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
#' @param assignment A named \code{list} of functions. These functions are provided, field_name,
#'   label, description and field_type and return a list of attributes to assign
#'   to the column. Defaults to creating a label attribute from the stripped
#'   HTML and UNICODE raw label and scanning for units={"UNITS"} in description
#'   to use as a units attribute.
#' @param config named \code{list}. Additional configuration parameters to pass to \code{httr::POST},
#'   These are appended to any parameters in \code{rcon$config}
#' @param api_param named \code{list}. Additional API parameters to pass into the body of the
#'   API call. This provides users to execute calls with options that may not
#'   otherwise be supported by redcapAPI.
#' @param csv_delimiter character. One of \code{c(",", "\t", ";", "|", "^")}. Designates the
#'   delimiter for the CSV file received from the API.
#' @param ... Argument to pass to other methods. 
#' 
#' @details Note about export rights: Please be aware that Data Export user 
#' rights will be applied to this API request. For example, if you have 
#' 'No Access' data export rights in the project, then the API report export 
#' will fail and return an error. And if you have 'De-Identified' or 'Remove 
#' All Identifier Fields' data export rights, then some data fields *might* 
#' be removed and filtered out of the data set returned from the API. 
#' To make sure that no data is unnecessarily filtered out of your API 
#' request, you should have 'Full Data Set' export rights in the project.
#' 
#' @export

exportReportsTyped <- function(rcon, 
                               report_id, 
                               ...){
  UseMethod("exportReportsTyped")
}

#' @rdname exportReportsTyped
#' @export

exportReportsTyped.redcapApiConnection <- function(rcon, 
                                                   report_id, 
                                                   drop_fields   = NULL, 
                                                   
                                                   # Type Casting Default Overrides Function Lists
                                                   na            = list(),
                                                   validation    = list(),
                                                   cast          = list(),
                                                   assignment    = list(label=stripHTMLandUnicode,
                                                                        units=unitsFieldAnnotation),
                                                   ..., 
                                                   config        = list(),
                                                   api_param     = list(),
                                                   csv_delimiter = ","){
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_integerish(x = report_id, 
                               len = 1, 
                               any.missing = FALSE, 
                               add = coll)
  
  checkmate::assert_character(x = drop_fields, 
                              any.missing = FALSE, 
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_list(x = na, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = validation, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = cast, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = cast, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = assignment, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         names = "named", 
                         add = coll)
  
  csv_delimiter <- checkmate::matchArg(x = csv_delimiter, 
                                       choices = c(",", "\t", ";", "|", "^"),
                                       .var.name = "csv_delimiter",
                                       add = coll)
  
  checkmate::reportAssertions(coll)
  
  MetaData <- rcon$metadata()
  ProjectFields <- rcon$fieldnames()
  available_fields <- unique(c(ProjectFields$original_field_name, 
                               ProjectFields$export_field_name, 
                               MetaData$field_name[MetaData$field_type %in% c("calc", "file")], 
                               REDCAP_SYSTEM_FIELDS))
  
  checkmate::assert_subset(x = drop_fields, 
                           choices = available_fields, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make the Body List                                           ####
  
  body <- list(content = "report", 
               report_id = report_id, 
               format = "csv", 
               returnFormat = "csv", 
               csvDelimiter = csv_delimiter)
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API                                                 ####
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config)
  
  Raw <- read.csv(text = as.character(response), 
                  na.strings = "", 
                  sep = csv_delimiter,
                  stringsAsFactors = FALSE)
  
  if (length(drop_fields) > 0){
    Raw <- Raw[!names(Raw) %in% drop_fields]
  }
  
  ###################################################################
  # Cast the fields in the report                                ####
  
  # See fieldCastingFunctions.R for definition of .castRecords
  .castRecords(Raw              = Raw, 
               Records          = NULL,
               rcon             = rcon, 
               na               = na, 
               validation       = validation, 
               cast             = cast, 
               assignment       = assignment, 
               default_cast     = .default_cast, 
               default_validate = .default_validate)
}

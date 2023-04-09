#' @name exportReports 
#' @title Export Reports from a REDCap Database
#' 
#' @description Exports reports from a REDCap Database and formats data if requested
#' 
#' @param rcon A REDCap connection object as created by \code{redcapConnection}.
#' @param report_id Integer.  Gives the report id of the desired report. 
#' This is located on the Report Builder page of the user interface on REDCap.
#' @param factors Logical.  Determines if categorical data from the database 
#' is returned as numeric codes or labelled factors.
#' @param labels Logical.  Determines if the variable labels are applied to the data frame.
#' @param dates Logical. Determines if date variables are converted to POSIXct format during the download.
#' @param checkboxLabels Logical. Determines the format of labels in checkbox 
#'   variables.  If \code{FALSE} labels are applies as "Unchecked"/"Checked".  
#'   If \code{TRUE}, they are applied as ""/"[field_labe]" where [field_label] 
#'   is the label assigned to the level in the data dictionary. This option 
#'   is only available after REDCap version 6.0.
#' @param drop An optional character vector of REDCap variable names to remove from the 
#'   dataset; defaults to NULL. E.g., \code{drop=c("date_dmy", "treatment")} 
#'   It is OK for drop to contain variables not present; these names are ignored.
#' @param ... Additional arguments to be passed between methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#' 
#' @details
#' A record of exports through the API is recorded in the Logging section of 
#' the project.
#' 
#' Reports are exported based on their id number, which can be looked up in 
#' the Reports page of a project
#' 
#' @section REDCap API Documentation (6.5.0):
#' This function allows you to export the data set of a report created on a project's 
#' "Data Exports, Reports, and Stats" page.
#' 
#' Note about export rights (6.0.0+): Please be aware that Data Export user rights will be 
#' applied to this API request. For example, if you have "No Access" data export rights 
#' in the project, then the API report export will fail and return an error. And if you 
#' have "De-Identified" or "Remove all tagged Identifier fields" data export rights, 
#' then some data fields *might* be removed and filtered out of the data set returned 
#' from the API. To make sure that no data is unnecessarily filtered out of your API 
#' request, you should have "Full Data Set" export rights in the project.
#' 
#' @section REDCap Version:
#' 6.0.0+
#' 
#' @section Known REDCap Limitations:
#' None
#' 
#' @author Benjamin Nutter
#' 
#' @export

exportReports <- function(rcon, 
                          report_id, 
                          factors        = TRUE, 
                          labels         = TRUE, 
                          dates          = TRUE, 
                          drop           = NULL,
                          checkboxLabels = FALSE, 
                          ...){
  UseMethod("exportReports")
}

#' @rdname exportReports
#' @export

exportReports.redcapApiConnection <- function(rcon, 
                                              report_id, 
                                              factors        = TRUE, 
                                              labels         = TRUE, 
                                              dates          = TRUE, 
                                              drop           = NULL, 
                                              checkboxLabels = FALSE, 
                                              ...,
                                              error_handling = getOption("redcap_error_handling"),
                                              config         = list(), 
                                              api_param      = list()){
  
  if (!is.numeric(report_id)) report_id <- as.numeric(report_id)
  
   ##################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_integerish(x = report_id,
                               len = 1,
                               add = coll)
  
  checkmate::assert_logical(x = factors, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = labels, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = dates, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = checkboxLabels, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_character(x = drop, 
                              any.missing = FALSE,
                              null.ok = TRUE, 
                              add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling, 
                                        choices = c("null", "error"),
                                        .var.name = "error_handling",
                                        add = coll)
  
  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         names = "named", 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
   ##################################################################
  # Get required information
  
  MetaData <- rcon$metadata()

  #* for purposes of the export, we don't need the descriptive fields. 
  #* Including them makes the process more error prone, so we'll ignore them.
  MetaData <- MetaData[!MetaData$field_type %in% "descriptive", ]  
  
  version <- rcon$version()

   ##################################################################
  # Make API Body List
  
  body <- list(token = rcon$token, 
               content = 'report',
               format = 'csv', 
               returnFormat = 'csv',
               report_id = report_id)
  
  body <- body[lengths(body) > 0]
  
   ##################################################################
  # Call the API
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) redcap_error(response, error_handling)
  
  Report <- utils::read.csv(text = as.character(response), 
                            stringsAsFactors = FALSE, 
                            na.strings = "")
  
   ##################################################################
  # Process the data

  #* synchronize underscore codings between records and meta data
  #* Only affects calls in REDCap versions earlier than 5.5.21
  if (utils::compareVersion(version, "6.0.0") == -1) 
    MetaData <- syncUnderscoreCodings(Report, MetaData)
  

  Report <- fieldToVar(records = Report, 
                       meta_data = MetaData, 
                       factors = factors, 
                       dates = dates, 
                       labels=labels,
                       checkboxLabels = checkboxLabels,
                       ...)
  
  
  if (labels) 
  {
    field_names <- names(Report)
    field_names <- unique(sub("___.+$", "", field_names))
    
    # For reports, there is not check on the field names, since 
    # the user may only select fields using the interface.
    # However, [form]_complete fields do not appear in the 
    # meta data and need to be removed to avoid an error.
    # See #108
    field_names <- field_names[field_names %in% MetaData$field_name]

    suffixed <- checkbox_suffixes(fields = field_names,
                                  meta_data = MetaData)

    Report[suffixed$name_suffix] <-
      mapply(nm = suffixed$name_suffix,
             lab = suffixed$label_suffix,
             FUN = function(nm, lab){
               if(is.null(Report[[nm]])){
                 warning("Missing field for suffix ", nm)
               } else {
                 labelVector::set_label(Report[[nm]], lab)
               }
             },
             SIMPLIFY = FALSE)
  }
  
   ##################################################################
  # Drop fields from Report
  
  if(length(drop)) {
    Report <- Report[!names(Report) %in% drop]
  } # end drop
  
  Report
}

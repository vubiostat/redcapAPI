#' @name exportReports 
#' @title Export Reports from a REDCap Database
#' 
#' @description Exports reports from a REDCap Database and formats data if requested
#' 
#' @param rcon A REDCap connection object as created by \code{redcapConnection}.
#' @param report_id Integer.  Gives the report id of the desired report. 
#' This is located on the Report Builder page of the user interface on REDCap.
#' @param raw_or_label One of \code{c("raw", "label")}. export the raw coded 
#'   values or labels for the options of multiple choice fields
#' @param raw_or_label_headers One of \code{c("raw", "label")}. export the 
#'   variable field names (\code{"raw"}) or the labels (\code{"label"}).
#' @param export_checkbox_label \code{logical(1)} specifies the format of 
#'   checkbox field values specifically when exporting the data as labels 
#'   (i.e., when \code{rawOrLabel = "label"}). When exporting labels, by 
#'   default (\code{FALSE}), all checkboxes will either have a value 
#'   'Checked' if they are checked or 'Unchecked' if not checked. 
#'   But if \code{TRUE}, it will instead export the checkbox value as the 
#'   checkbox option's label (e.g., 'Choice 1') if checked or it will be 
#'   blank/empty (no value) if not checked.
#' @param csv_delimiter \code{character}. One of 
#'   \code{c(",", "\t", ";", "|", "^")}. Designates the delimiter for the CSV
#'   file received from the API.
#' @param ... Additional arguments to be passed between methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcapError}}
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
#' @author Benjamin Nutter
#' 
#' @export

exportReports <- function(rcon, 
                          report_id, 
                          raw_or_label          = c("raw", "label"),
                          raw_or_label_headers  = c("raw", "label"),
                          export_checkbox_label = FALSE,  
                          ...){
  UseMethod("exportReports")
}

#' @rdname exportReports
#' @export

exportReports.redcapApiConnection <- function(rcon, 
                                              report_id, 
                                              raw_or_label          = c("raw", "label"),
                                              raw_or_label_headers  = c("raw", "label"),
                                              export_checkbox_label = FALSE,  
                                              ...,
                                              csv_delimiter         = c(",", "\t", ";", "|", "^"), 
                                              error_handling = getOption("redcap_error_handling"),
                                              config         = list(), 
                                              api_param      = list()){
  
  if (is.character(report_id)) report_id <- as.numeric(report_id)
  
  ###################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_integerish(x = report_id,
                               len = 1,
                               add = coll)
  
  raw_or_label <- checkmate::matchArg(x = raw_or_label, 
                                      choices = c("raw", "label"), 
                                      add = coll, 
                                      .var.name = "raw_or_label")
  
  raw_or_label_headers <- checkmate::matchArg(x = raw_or_label_headers, 
                                              choices = c("raw", "label"), 
                                              add = coll, 
                                              .var.name = "raw_or_label_headers")
  
  checkmate::assert_logical(x = export_checkbox_label, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  csv_delimiter <- checkmate::matchArg(x = csv_delimiter, 
                                       choices = c(",", "\t", ";", "|", "^"), 
                                       add = coll, 
                                       .var.name = "csv_delimiter")
  
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
  
  ###################################################################
  # Make the Body List                                           ####
  
  body <- list(content = "report", 
               format = "csv", 
               returnFormat = "csv",
               report_id = report_id,
               rawOrLabel = raw_or_label, 
               rawOrLabelHeaders = raw_or_label_headers,
               exportCheckboxLabel = tolower(export_checkbox_label), 
               csvDelimiter = csv_delimiter)
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Make the API Call
  
  response <- makeApiCall(rcon, 
                          body = c(body, 
                                   api_param), 
                          config = config)
  
  if (response$status_code != 200){
    redcapError(response, 
                error_handling = error_handling)
  }
  
  read.csv(text = as.character(response), 
           stringsAsFactors = FALSE, 
           na.strings = "")
}

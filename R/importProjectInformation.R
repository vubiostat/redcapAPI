#' @name importProjectInformation
#' @title Import Project Information
#' 
#' @description This method allows you to update some of the basic 
#'   attributes of a given REDCap project, such as the project's title, 
#'   if it is longitudinal, if surveys are enabled, etc. Its data format 
#'   corresponds to the format in the API method Export Project Information.
#'   
#' @param rcon a \code{redcapConnection} object. 
#' @param data \code{data.frame} with only one row and any subset of allowable fields to be
#'   updated. See Details.
#' @param refresh \code{logical(1)}. If \code{TRUE}, the cached project information
#'   will be updated after the import.
#' @param ... Additional arguments to pass to other methods.
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
#'   Fields that are not editable will be quietly removed prior to 
#'   import. This allows the user to use the result of 
#'   \code{exportProjectInformation} as a template for the import.
#'   
#'   Attributes for the project in the format specified. For any values
#'   that are boolean, they should be represented as either a '0' (no/false) 
#'   or '1' (yes/true). The following project attributes can be udpated:
#' 
#' \itemize{
#'  \item{\code{project_title}}
#'  \item{\code{project_language}}
#'  \item{\code{purpose}}
#'  \item{\code{purpose_other}}
#'  \item{\code{project_notes}}
#'  \item{\code{custom_record_label}}
#'  \item{\code{secondary_unique_field}}
#'  \item{\code{is_longitudinal}}
#'  \item{\code{surveys_enabled}}
#'  \item{\code{scheduling_enabled}}
#'  \item{\code{record_autonumbering_enabled}}
#'  \item{\code{randomization_enabled}}
#'  \item{\code{project_irb_number}}
#'  \item{\code{project_grant_number}}
#'  \item{\code{project_pi_firstname}}
#'  \item{\code{project_pi_lastname}}
#'  \item{\code{display_today_now_button}}
#'  \item{\code{bypass_branching_erase_field_prompt}}
#' }
#'
#' @export

importProjectInformation <- function(rcon, 
                                     data, 
                                     ...){
  UseMethod("importProjectInformation")
}

#' @rdname importProjectInformation
#' @export

importProjectInformation.redcapApiConnection <- function(rcon, 
                                                         data, 
                                                         refresh = TRUE, 
                                                         ...,
                                                         error_handling = getOption("redcap_error_handling"), 
                                                         config         = list(), 
                                                         api_param      = list()){
  
  ###################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = data,
                               nrows = 1,
                               add = coll)
  
  checkmate::assert_logical(x = refresh, 
                            len = 1, 
                            any.missing = FALSE, 
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
  
  # Remove uneditable fields.
  data <- data[!names(data) %in% PROJECT_INFO_FIELDS_FIXED]
  
  checkmate::assert_subset(x = names(data), 
                           choices = PROJECT_INFO_FIELDS_EDITABLE, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  .importProjectInformation_validateDataFields(data, coll)
  
  ###################################################################
  # Make API Body List
  
  body <- list(content = "project_settings", 
               format = "csv", 
               data = writeDataForImport(data))
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  
  if (response$status_code != 200) return(redcap_error(response, error_handling))
  
  message(sprintf("Fields updated: %s", 
                  as.character(response)))
  
  if (refresh && rcon$has_projectInformation()){
    rcon$refresh_projectInformation()
  }
}

#####################################################################
# Unexported

.importProjectInformation_validateDataFields <- function(data, 
                                                         coll){
  if ("project_title" %in% names(data)){
    checkmate::assert_character(data$project_title, 
                                add = coll, 
                                .var.name = "project_title")
  }
  
  if ("project_language" %in% names(data)){
    checkmate::assert_character(data$project_language, 
                                add = coll, 
                                .var.name = "project_language")
  }
  
  if ("purpose" %in% names(data)){
    checkmate::assert_integerish(x = data$purpose, 
                                 add = coll, 
                                 .var.name = "purpose")
  }
  
  if ("purpose_other" %in% names(data)){
    checkmate::assert_character(x = data$purpose_other, 
                                add = coll, 
                                .var.name = "purpose_other")
  }
  
  if ("project_notes" %in% names(data)){
    checkmate::assert_character(x = data$project_notes, 
                                add = coll, 
                                .var.name = "project_notes")
  }
  
  if ("custom_record_label" %in% names(data)){
    checkmate::assert_character(x = data$custom_record_label, 
                                add = coll, 
                                .var.name = "custom_record_label")
  }
  
  if ("secondary_unique_field" %in% names(data)){
    checkmate::assert_character(x = data$secondary_unique_field, 
                                add = coll, 
                                .var.name = "secondary_unique_field")
  }
  
  if ("is_longitudinal" %in% names(data)){
    if (!all(data$is_longitudinal %in% c(0, 1))){
      coll$push("'is_longitudinal' must be 0/1")
    }
  }
  
  if ("surveys_enabled" %in% names(data)){
    if (!all(data$surveys_enabled %in% c(0, 1))){
      coll$push("'surveys_enabled' must be 0/1")
    }
  }
  
  if ("scheduling_enabled" %in% names(data)){
    if (!all(data$scheduling_enabled %in% c(0, 1))){
      coll$push("'scheduling_enabled' must be 0/1")
    }
  }
  
  if ("record_autonumbering_enabled" %in% names(data)){
    if (!all(data$record_autonumbering_enabled %in% c(0, 1))){
      coll$push("'record_autonumbering_enabled' must be 0/1")
    }
  }
  
  if ("randomization_enabled" %in% names(data)){
    if (!all(data$randomization_enabled %in% c(0, 1))){
      coll$push("'randomization_enabled' must be 0/1")
    }
  }
  
  if ("project_irb_number" %in% names(data)){
    checkmate::assert_character(x = data$project_irb_number, 
                                add = coll, 
                                .var.name = "project_irb_number")
  }
  
  if ("project_grant_number" %in% names(data)){
    checkmate::assert_character(x = data$project_grant_number, 
                                add = coll, 
                                .var.name = "project_grant_number")
  }
  
  if ("project_pi_firstname" %in% names(data)){
    checkmate::assert_character(x = data$project_pi_firstname, 
                                add = coll, 
                                .var.name = "project_pi_firstname")
  }
  
  if ("project_pi_lastname" %in% names(data)){
    checkmate::assert_character(x = data$project_pi_lastname, 
                                add = coll, 
                                .var.name = "project_pi_lastname")
  }
  
  if ("display_today_now_button" %in% names(data)){
    if (!all(data$display_today_now_button %in% c(0, 1))){
      coll$push("'display_today_now_button' must be 0/1")
    }
  }
  
  if ("bypass_branching_erase_field_prompt" %in% names(data)){
    if (!all(data$bypass_branching_erase_field_prompt %in% c(0, 1))){
      coll$push("'bypass_branching_erase_field_prompt' must be 0/1")
    }
  }
  
  checkmate::reportAssertions(coll)
}

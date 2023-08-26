#' @name createRedcapProject
#' @title Create REDCap Project
#' 
#' @description This method allows you to create a new REDCap project. 
#'   A 64-character Super API Token is required for this method (as opposed 
#'   to project-level API methods that require a regular 32-character 
#'   token associated with the project-user). In the API request, you must 
#'   minimally provide the project attributes 'project_title' and 
#'   'purpose' (with numerical value 0=Practice/Just for fun, 1=Other, 
#'   2=Research, 3=Quality Improvement, 4=Operational Support) when 
#'   creating a project.
#'   
#'   When a project is created with this method, the project will 
#'   automatically be given all the project-level defaults just as if you
#'   created a new empty project via the web user interface, such as a 
#'   automatically creating a single data collection instrument seeded with a 
#'   single Record ID field and Form Status field, as well as 
#'   (for longitudinal projects) one arm with one event. And if you intend 
#'   to create your own arms or events immediately after creating the project, 
#'   it is recommended that you utilize the override=1 parameter in the 
#'   'Import Arms' or 'Import Events' method, respectively, so that the 
#'   default arm and event are removed when you add your own. Also, the user 
#'   creating the project will automatically be added to the project as a
#'    user with full user privileges and a project-level API token, 
#'    which could then be used for subsequent project-level API requests.
#'    
#'    NOTE: Only users with Super API Tokens can utilize this method. 
#'    Users can only be granted a super token by a REDCap administrator 
#'    (using the API Tokens page in the REDCap Control Center). Please be 
#'    advised that users with a Super API Token can create new REDCap projects 
#'    via the API without any approval needed by a REDCap administrator.
#'    If you are interested in obtaining a super token, please contact your 
#'    local REDCap administrator.
#'    
#' @param rcon A \code{redcapConnection} object.
#' @param project_title \code{character(1)} Title for the new project. 
#' @param purpose \code{character}, one of 
#'   \code{c("Practice/just for fun", "Other", "Research", "Quality Improvement", "Operational Support")}
#' @param purpose_other \code{character(1)} or \code{NULL}. Ignored unless 
#'   \code{purpose = "Other"}, in which case this becomes a required argument.
#' @param is_longitudinal \code{logical(1)}, determines if the project will 
#'   be set as a longitudinal project.
#' @param surveys_enabled \code{logical(1)}, determines if surveys are enabled
#'   for the project. (This will not add any survey instruments, only 
#'   enable them).
#' @param record_autonumbering_enabled \code{logical(1)}, determines if 
#'   auto numbering will be enabled in the project.
#' @param xml \code{character(1)} an XML string in CDISC ODM XML format that 
#'   contains project metadata (fields, forms, events, arms) and might 
#'   optionally contain data to be imported as well. The XML contained in 
#'   this parameter can come from a REDCap Project XML export file from 
#'   REDCap itself, or may come from another system that is capable of 
#'   exporting projects and data in CDISC ODM format. If the 'odm' parameter 
#'   is included in the API request, it will use the XML to import its 
#'   contents into the newly created project. This will allow you not only 
#'   to create the project with the API request, but also to import all fields, 
#'   forms, and project attributes (and events and arms, if longitudinal) as 
#'   well as record data all at the same time.
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
#' @export

createRedcapProject <- function(rcon, 
                                project_title, 
                                purpose = REDCAP_PROJECT_PURPOSE, 
                                purpose_other = NULL, 
                                is_longitudinal = FALSE,
                                surveys_enabled = FALSE, 
                                record_autonumbering_enabled = FALSE,
                                xml = NULL, 
                                ...){
  UseMethod("createRedcapProject")
}

#' @rdname createRedcapProject
#' @export

createRedcapProject.redcapApiConnection <- function(rcon, 
                                                    project_title, 
                                                    purpose = REDCAP_PROJECT_PURPOSE, 
                                                    purpose_other = NULL, 
                                                    is_longitudinal = FALSE,
                                                    surveys_enabled = FALSE, 
                                                    record_autonumbering_enabled = FALSE, 
                                                    xml = NULL, 
                                                    ...,
                                                    error_handling = getOption("redcap_error_handling"),
                                                    config         = list(), 
                                                    api_param      = list()){

  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapConnection", 
                          add = coll)
  
  checkmate::assert_character(x = project_title, 
                              len = 1, 
                              any.missing = FALSE, 
                              add = coll)
  
  purpose <- checkmate::matchArg(x = purpose, 
                                 choices = REDCAP_PROJECT_PURPOSE, 
                                 add = coll, 
                                 .var.name = "purpose")
 
  checkmate::assert_character(x = purpose_other, 
                              len = 1, 
                              null.ok = isTRUE(!purpose %in% "Other"), 
                              add = coll)
  
  checkmate::assert_logical(x = is_longitudinal, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_logical(x = surveys_enabled, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_logical(x = record_autonumbering_enabled, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_character(x = xml, 
                              len = 1, 
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
  
  checkmate::assert_character(x = rcon$token, 
                              n.chars = 64, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make the Project Settings Data Frame
  
  purpose <- match(purpose, REDCAP_PROJECT_PURPOSE) - 1
  
  data <- data.frame(project_title = project_title, 
                     purpose = purpose, 
                     purpose_other = if (is.null(purpose_other)) "" else purpose_other, 
                     is_longitudinal = as.numeric(is_longitudinal), 
                     surveys_enabled = as.numeric(surveys_enabled), 
                     record_autonumbering_enabled = as.numeric(record_autonumbering_enabled), 
                     stringsAsFactors = FALSE)
  
  ###################################################################
  # API Body List                                                ####
  
  body <- list(content = "project", 
               format = "csv", 
               data = writeDataForImport(data), 
               returnFormat = "csv", 
               odm = xml)
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API 
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) redcapError(response, error_handling)
  
  as.character(response)
}

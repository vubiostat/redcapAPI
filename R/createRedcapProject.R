#' @name createRedcapProject
#' @title Create REDCap Project
#' 
#' @description These methods enable a user with a 64-character Super API
#'   token to create a new REDCap project. 
#'    
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param project_title `character(1)`. Title for the new project. 
#' @param purpose `character`, one of 
#'   `c("Practice/just for fun", "Other", "Research", "Quality Improvement", "Operational Support")`
#' @param purpose_other `character(1)` or `NULL`. Ignored unless 
#'   `purpose = "Other"`, in which case this becomes a required argument.
#' @param is_longitudinal `logical(1)`. When `TRUE` the project will 
#'   be set as a longitudinal project.
#' @param surveys_enabled `logical(1)`. When `TRUE` surveys are enabled
#'   for the project. (This will not add any survey instruments, only 
#'   enable them).
#' @param record_autonumbering_enabled `logical(1)`. When `TRUE` if 
#'   auto numbering will be enabled in the project.
#' @param xml `character(1)` or `NULL` an XML string in CDISC ODM XML format that 
#'   contains project metadata (fields, forms, events, arms) and might 
#'   optionally contain data to be imported as well. When not `NULL`, 
#'   all other arguments are ignored. See Details. 
#'   
#' @details
#'   The user creating the project will automatically be added to the project as a
#'   user with full user privileges and a project-level API token, 
#'   which could then be used for subsequent project-level API requests.
#'   
#'   When the project is created, it will automatically be given all the 
#'   project-level defaults just as if it had been created via the web user 
#'   interface, such as automatically creating a single data collection 
#'   instrument seeded with a single Record ID field and Form Status field, 
#'   as well as (for longitudinal projects) one arm with one event. 
#'   
#'   If the user intends to populate the project with arms and events 
#'   immediately after creating the project, it is recommended that 
#'   `override = TRUE` be used in `importArms` and `importEvents` so that the 
#'   default arm and event are removed. 
#'   
#'   The `xml` argument must be in CDISC ODM XML format. It may come from a 
#'   REDCap Project XML export  file from REDCap itself 
#'   (see [exportProjectXml()]), or may come from another system that is capable of 
#'   exporting projects and data in CDISC ODM format. If the `xml` argument 
#'   is used in the API request, it will use the XML to import its 
#'   contents into the newly created project. This will not only 
#'   create the project with the API request, but also to import all fields, 
#'   forms, and project attributes (and events and arms, if longitudinal) as 
#'   well as record data all at the same time.
#'    
#'   Only users with a 64-character Super API Tokens can utilize this method
#'   (the standard API token is 32 characters). Users can only be granted a 
#'   super token by a REDCap administrator (using the API Tokens page in the 
#'   REDCap Control Center). Please be advised that users with a Super API 
#'   Token can create new REDCap projects via the API without any approval 
#'   needed by a REDCap administrator.
#' 
#' @return
#' Returns a `character(1)` the 32-character, project level API token 
#' assigned to the user that created the project. This token is intended 
#' to be used for further project configuration using the API. 
#'
#' @seealso 
#' [exportProjectXml()]
#' 
#' @examples
#' \dontrun{
#' # The token must be a 64-character token
#' super_token <- redcapConnection(url = "your_redcap_url", 
#'                                 token = "[64-character-super-api-token]")
#'          
#' # Create a new project    
#' createRedcapProject(super_token, 
#'                     project_title = "New Project Name", 
#'                     purpose = "Quality Improvement", 
#'                     is_longitudinal = FALSE, 
#'                     surveys_enabled = TRUE)
#'                     
#'                     
#'                     
#' # Copy an existing project into a new project
#' unlockREDCap(connections = c(rcon = "token_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' xml_file <- tempfile(file.ext = ".xml")
#' exportProjectXml(rcon, 
#'                  file = xml_file)
#'  
#' xml_text <- paste0(readLines(xml_file), collapse = " ")
#' createRedcapProject(super_token, 
#'                     xml = xml_text)
#' }
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
                                                    purpose         = REDCAP_PROJECT_PURPOSE, 
                                                    purpose_other   = NULL, 
                                                    is_longitudinal = FALSE,
                                                    surveys_enabled = FALSE, 
                                                    record_autonumbering_enabled = FALSE, 
                                                    xml             = NULL, 
                                                    ...)
{

  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapConnection", 
                          add = coll)
  
  if (is.null(xml)){
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
  } else {  
    checkmate::assert_character(x = xml, 
                                len = 1, 
                                null.ok = TRUE, 
                                add = coll)
  }

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

  ###################################################################
  # Call the API 
  as.character(makeApiCall(rcon, body, ...))
}

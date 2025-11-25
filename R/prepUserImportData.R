#' @name prepUserImportData
#' @title Prepare User Data for Import
#'
#' @description Prepares a data frame for import via the API. Allows for
#'   data to be passed in either the raw format or the labeled data
#'   received from `exportUsers`.
#'
#' @inheritParams common-rcon-arg
#' @param data `data.frame` with the structure of
#'   `redcapAPI:::REDCAP_USER_STRUCTURE`. It may also have additional
#'   columns for the form and export access of each of the instruments.
#' @param consolidate `logical(1)` If `TRUE`, the form and data
#'   export access values will be read from the expanded columns. Otherwise,
#'   the consolidated values (as provided by the API export) are utilized.
#' @param user_role `logical(1)` If `TRUE`, the code will
#'   treat the data as if it is being prepared for importing User Roles.
#'
#' @return
#' Returns a `data.frame` with user settings that will be accepted by the
#' API for import.
#'
#' @seealso
#' [importUsers()], \cr
#' [importUserRoles()]
#'
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"),
#'              url = "your_redcap_url",
#'              keyring = "API_KEYs",
#'              envir = globalenv())
#'
#'
#' # Prep user data
#' NewData <- data.frame(username = "target_user",
#'                       design = 1,
#'                       api_export = "Access",
#'                       api_import = "No Access",
#'                       surveys_enabled = 0)
#' prepUserImportData(data = NewData,
#'                    rcon = rcon)
#'
#' # Prep user role data
#' NewData <- data.frame(unique_role_name = "target_user",
#'                       design = 1,
#'                       api_export = "Access",
#'                       api_import = "No Access",
#'                       surveys_enabled = 0)
#' prepUserImportData(data = NewData,
#'                    rcon = rcon)
#' }
#'
#' @export

prepUserImportData <- function(data,
                               rcon,
                               consolidate = TRUE,
                               user_role = FALSE)
{
  ###################################################################
  # Argument Validation                                          ####

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_data_frame(x = data,
                               col.names = "named",
                               add = coll)

  checkmate::assert_class(x = rcon,
                          classes = "redcapConnection",
                          add = coll)

  checkmate::assert_logical(x = consolidate,
                            len = 1,
                            add = coll)

  checkmate::assert_logical(x = user_role,
                            len = 1,
                            add = coll)

  checkmate::reportAssertions(coll)

  all_fields <- names(data)
  form_access_field <- grep('_form_access$', all_fields, value = TRUE)
  export_access_field <- grep('_export_access$', all_fields, value = TRUE)
  primary_fields <- setdiff(all_fields, c(form_access_field, export_access_field))

  checkmate::assert_subset(x = primary_fields,
                           choices = c(if (user_role) names(redcapUserRoleStructure(rcon$version())) else names(redcapUserStructure(rcon$version())),
                                       "data_export"),
                           add = coll)

  checkmate::reportAssertions(coll)

  # Prior to redcapAPI version 2.11.5, functionality surrounding form validation
  # did not work properly. See GH issue #474.
  # Specifically user permissions for forms and forms export were checked against
  # the list of all instruments. The prior design was to generate an error
  # if one of the instruments was not present.
  # The current design is to let REDCap API handle the missing instruments,
  # which will set the permissions to "No Access".
  # The functions "prepUserImportData_validateAllFormsPresent" and
  # "prepUserImportData_extractFormName" were removed with this change.

  ###################################################################
  # Functional Code                                              ####

  # Convert Expiration to text string, if necessary

  if (inherits(data$expiration, "Date") || inherits(data$expiration, "POSIXct")){
    data$expiration <- format(data$expiration,
                              format = "%Y-%m-%d")
  }

  # Remove fields that cannot be updated

  fields_to_remove <- c("email", "lastname", "firstname",
                        "data_access_group_id", "data_access_group") #?, "data_access_groups")
  data <- data[!names(data) %in% fields_to_remove]

  # Convert values to numeric

  for (nm in names(data)){
    data[[nm]] <-
      if (nm == 'data_access_group'){
        # as of version 2.11.5, DAG is in "fields_to_remove"
        # this chunk will never be run
        # in the future we may handle it, so leaving the information below

        # don't convert DAG into numeric
        # it qualifies as REDCAP_USER_TABLE_ACCESS_VARIABLES
        # possibly convert to numeric but leave NA?
        data[[nm]]
      } else if (nm %in% REDCAP_USER_TABLE_ACCESS_VARIABLES){
        prepUserImportData_castAccessVariable(data[[nm]])
      } else if (nm %in% form_access_field){
        prepUserImportData_castFormAccess(rcon, data[[nm]])
      } else if (nm %in% export_access_field){
        prepUserImportData_castExportAccess(data[[nm]])
      } else {
        data[[nm]]
      }
  }

  # Make Form Access Field

  if (consolidate){
    data$forms <-
      prepUserImportData_consolidateAccess(data[form_access_field],
                                           "_form_access$")

    data$forms_export <-
      prepUserImportData_consolidateAccess(data[export_access_field],
                                           "_export_access$")
  }

  data <- data[!names(data) %in% c(form_access_field,
                                   export_access_field)]

  data
}

#####################################################################
# Unexported                                                     ####

prepUserImportData_castAccessVariable <- function(x){
  as.numeric(x %in% c(1, "Access"))
}

prepUserImportData_castExportAccess <- function(x){
  x <- as.character(x)

  to_zero  <- which(x %in% c(0, "No Access"))
  to_one   <- which(x %in% c(1, "Full Data Set"))
  to_two   <- which(x %in% c(2, "De-Identified"))
  to_three <- which(x %in% c(3, "Remove Identifier Fields"))

  x[to_zero]  <- rep(0, length(to_zero))
  x[to_one]   <- rep(1, length(to_one))
  x[to_two]   <- rep(2, length(to_two))
  x[to_three] <- rep(3, length(to_three))

  as.numeric(x)
}

#' @importFrom utils compareVersion
prepUserImportData_castFormAccess <- function(rcon, x)
{
  map <- if(utils::compareVersion("15.6.0", rcon$version()) == 1)
  {
   c("No Access"=0,
     "0"=0,
     "Read Only"=2,
     "2"=2,
     "View survey responses and Edit records"=1,
     "1"=1,
     "Edit survey responses and records"=3,
     "3"=3)
  } else
  {
   c("No Access"=128,
     "128"=128,
     "0"=128,
     "Read Only"=129,
     "129"=129,
     "2"=129,
     "View survey responses and Edit records"=130,
     "130"=130,
     "1"=130,
     "Edit survey responses and records"=138,
     "138"=138,
     "3"=138,
     "View survey responses and Edit or Delete records"=146,
     "146"=146,
     "Edit or Delete Survey responses and records"=154,
     "154"=154)
  }
  # NA values are not handled here by design
  # values not found in "map" will also return NA
  map[as.character(x)]
}

prepUserImportData_consolidateAccess <- function(d, suffix)
{
  for (i in seq_along(d))
  {
    this_name <- sub(suffix, "", names(d)[i])
    d[[i]] <- ifelse(is.na(d[[i]]), NA, sprintf("%s:%s", this_name, d[[i]]))
  }

  apply(d, MARGIN = 1, FUN = function(x) paste0(x[!is.na(x)], collapse = ","))
}

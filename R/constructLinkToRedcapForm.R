#' @name constructLinkToRedcapForm
#' @title Construct a Link to a REDCap Form
#'
#' @description Uses information from the project and a record to link to
#'   the form on which a data element is recorded. This is intended to be used
#'   within the report of invalid results when exporting or importing
#'   records. It should be noted that when importing records, the records
#'   may not yet exist and the links may not work.
#'
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @param form_name `character`. The name of the form on which the field
#'   name exists.
#' @param record_id `character`. The ID of the record being linked to.
#'   If passed as a numeric value, it will be coerced to character.
#'   Must have the same length as `form_name`.
#' @param event_id `character` or `NULL`. For classical projects, use either
#'   `NULL` or `NA` (`NA` support is permitted to assist with vectorization).
#'   For longitudinal projects, the ID of the unique event. If passed
#'   as a numeric value, it will be coerced to character.
#'
#' @details Constructing a link to a REDCap form requires knowledge of the
#'   following:
#'
#' * The REDCap instance url (usually 'redcap.institution.domain').
#' * The REDCap instance version number.
#' * The REDCap project ID number
#' * The record ID
#' * The form name
#' * The event ID number (if the project is longitudinal).
#'
#' If any of these items in unknown, a missing value will be returned. For
#' `redcapOfflineConnection`s, the user will need to provide the version
#' number, the project information, and the events (if the project
#' is longitudinal) as part of the call to `offlineConnection`. Note that
#' the REDCap User Interface does not include the event ID number with the
#' file download for events.
#'
#' @return Returns a character vector the same length of `form_name`.

constructLinkToRedcapForm <- function(rcon,
                                      form_name,
                                      record_id,
                                      event_id = NULL,
                                      ...){
  UseMethod("constructLinkToRedcapForm")
}

#' @rdname constructLinkToRedcapForm
#' @export

constructLinkToRedcapForm.redcapApiConnection <- function(rcon,
                                                          form_name,
                                                          record_id,
                                                          event_id = NULL,
                                                          ...){
  if (is.numeric(record_id)) record_id <- as.character(record_id)
  if (is.numeric(event_id)) event_id <- as.character(event_id)

  ###################################################################
  # Argument Validation                                          ####
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)

  checkmate::assert_character(x = form_name,
                              add = coll)

  checkmate::assert_character(x = record_id,
                              any.missing = FALSE,
                              add = coll)

  checkmate::assert_character(x = event_id,
                              any.missing = TRUE,
                              null.ok = TRUE,
                              add = coll)

  checkmate::reportAssertions(coll)

  if (is.null(event_id)){
    event_id <- rep(NA_character_, length(form_name))
  }

  arg_lengths <- lengths(list(form_name,
                              record_id,
                              event_id))
  if (length(unique(arg_lengths)) != 1){
    coll$push("'form_name', 'record_id', and 'event_id' must all have the same length.")
  }

  checkmate::reportAssertions(coll)

  ###################################################################
  # Functionality                                                ####

  if (rcon$projectInformation()$is_longitudinal == 1 &&
      any(is.na(event_id))){
    logWarning("Links to REDCap forms not provided for longitudinal projects with missing event_id.")
  }

  is_complete <-
    if (rcon$projectInformation()$is_longitudinal == 0){
      !is.na(form_name) & !is.na(record_id)
    } else {
      !is.na(form_name) & !is.na(record_id) & !is.na(event_id)
    }

  url <- .constructLinkToRedcapForm_makeLink(url = rcon$url,
                                             version = rcon$version(),
                                             project_id = rcon$projectInformation()$project_id,
                                             form_name = form_name,
                                             record_id = record_id,
                                             event_id = event_id)

  url[!is_complete] <- NA_character_

  url
}

#' @rdname constructLinkToRedcapForm
#' @export

constructLinkToRedcapForm.redcapOfflineConnection <- function(rcon,
                                                              form_name,
                                                              record_id,
                                                              event_id = NULL,
                                                              ...){
  if (is.numeric(record_id)) record_id <- as.character(record_id)
  if (is.numeric(event_id)) event_id <- as.character(event_id)

  ###################################################################
  # Argument Validation                                          ####
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapOfflineConnection",
                          add = coll)

  checkmate::assert_character(x = form_name,
                              add = coll)

  checkmate::assert_character(x = record_id,
                              any.missing = FALSE,
                              add = coll)

  checkmate::assert_character(x = event_id,
                              any.missing = TRUE,
                              null.ok = TRUE,
                              add = coll)

  checkmate::reportAssertions(coll)

  if (is.null(event_id)){
    event_id <- rep(NA_character_, length(form_name))
  }

  arg_lengths <- lengths(list(form_name,
                              record_id,
                              event_id))
  if (length(unique(arg_lengths)) != 1){
    coll$push("'form_name', 'record_id', and 'event_id' must all have the same length.")
  }

  checkmate::reportAssertions(coll)

  ###################################################################
  # Functionality                                                ####

  if (is.null(rcon$url)){
    logWarning("REDCap URL is not stored on the offlineConnection object. Links to REDCap forms will not work as desired.")
    return(rep(NA_character_, length(form_name)))
  }

  if (is.null(rcon$version())){
    logWarning("Version number not stored on the offlineConnection object. Links to REDCap forms will not work as desired.")
    return(rep(NA_character_, length(form_name)))
  }

  if (any(is.na(event_id))){
    if (is.null(rcon$projectInformation())){
      logWarning("Project information is not stored on the offlineConnection object. Links to REDCap forms will not work as desired if the project is longitudinal")
    } else if (isTRUE(rcon$projectInformation()$is_longitundal == 1)){
      logWarning("Event ID not provided for a longitudinal project. Links to REDCap forms will not work as desired.")
    }
  }

  is_complete <-
    if (is.null(rcon$projectInformation())){
      !is.na(form_name) & !is.na(record_id)
    } else if (rcon$projectInformation()$is_longitudinal == 0){
      !is.na(form_name) & !is.na(record_id)
    } else {
      !is.na(form_name) & !is.na(record_id) & !is.na(event_id)
    }

  url <- .constructLinkToRedcapForm_makeLink(url = rcon$url,
                                             version = rcon$version(),
                                             project_id = rcon$projectInformation()$project_id,
                                             form_name = form_name,
                                             record_id = record_id,
                                             event_id = event_id)
    url[!is_complete] <- NA_character_

  url
}

#####################################################################
# Unexported                                                     ####

.constructLinkToRedcapForm_makeLink <- function(url,
                                                version,
                                                project_id,
                                                form_name,
                                                record_id,
                                                event_id){
  url <- sprintf("%s/redcap_v%s/DataEntry/index.php?pid=%s&page=%s&id=%s",
                 sub("/api(/|)$", "", url),
                 version,
                 project_id,
                 form_name,
                 record_id)

  url <-
    ifelse(!is.na(event_id),
           sprintf("%s&event_id=%s",
                   url,
                   event_id),
           url)

  url
}

#' @name missingSummary
#' @aliases missingSummary_offline
#' 
#' @title Report of Missing Values
#' @description Returns a data frame of subject events with missing values. 
#' 
#' @inheritParams common-rcon-arg
#' @inheritParams common-api-args
#' @param records `character(1)` A filename pointing to the raw records
#'   download from REDCap.
#' @param meta_data `character(1)` A filename pointing to the data dictionary 
#'   download from REDCap.
#' @param excludeMissingForms `logical(1)` When `TRUE`, forms where all 
#'   fields are missing are assumed to be deliberately missing data and 
#'   are excluded from the count of missing values. An example when this is
#'   desirable is if a patient did not experience an adverse event; 
#'   the adverse event form would contain no data and the empty fields
#'   should not be considered missing data.
#' @param ... additional arguments passed to inner call of exportRecordsTyped.
#'   
#' @details The intention of this function is to generate a list of subject
#'   events that are missing and could potentially be values that should have
#'   been entered.
#'   
#'   The branching logic from the data dictionary is parsed and translated into
#'   and R expression.  When a field with branching logic passes the logical
#'   statement, it is evaluated with `is.na`, otherwise, it is set to 
#'   `FALSE` (non-missing, because there was never an opportunity to 
#'   provide a value).  The utility of this function is limited to simple 
#'   logic where all of the
#'   data exist within the same row. Any complex statements using events 
#'   will result in a failure. 
#'   
#'   Optionally, forms that are entirely missing can be determined to be 
#'   non-missing.  This is applicable when, for instance, a patient did not 
#'   have an adverse event.  In this case, a form dedicated to adverse events 
#'   would contain meaningless missing values and could be excluded from the 
#'   report.
#'   
#' @seealso 
#' `vignette("redcapAPI-offline-connection", package = "redcapAPI")`
#'   
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' # Generate a summary of missing values for the entire project
#' missingSummary(rcon)
#' 
#' # Generate a summary of missing values for a single form
#' missingSummary(rcon, 
#'                exportRecordsArgs = list(forms = "target_form"))
#' }
#' 
#' @export

missingSummary <- function(rcon, 
                           excludeMissingForms = TRUE, 
                           ...)
{
  UseMethod("missingSummary")
}

#' @rdname missingSummary
#' @export

missingSummary.redcapApiConnection <- function(rcon, 
                                               excludeMissingForms = TRUE, 
                                               ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  checkmate::assert_logical(x = excludeMissingForms, 
                            len = 1, 
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  RecordsOrig <- exportRecordsTyped(rcon, cast=raw_cast,...)
  
  # Import the Meta Data --------------------------------------------
  MetaData <- rcon$metadata()
  MetaData <- MetaData[MetaData$field_type != "descriptive", ]
  
  logic <- parseBranchingLogic(MetaData$branching_logic)
  names(logic) <- MetaData$field_name
  
  Records <- .missingSummary_isMissingInField(RecordsOrig, 
                                              MetaData, 
                                              logic)
  
  if (excludeMissingForms){
    Records <- .missingSummary_excludeMissingForm(Records, 
                                                  MetaData, 
                                                  logic)
  }
  
  .missingSummary_makeResultFrame(Records, 
                                  MetaData)
}

#' @rdname missingSummary
#' @export

missingSummary_offline <- function(records, 
                                   meta_data, 
                                   excludeMissingForms = TRUE){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_file_exists(x = records, 
                                add = coll)
  
  checkmate::assert_file_exists(meta_data, 
                                add = coll)
  
  checkmate::assert_logical(x = excludeMissingForms, 
                            len = 1, 
                            add = coll)
  
  records_orig <- utils::read.csv(records,
                                  stringsAsFactors=FALSE, 
                                  na.string="")
  
  meta_data <- 
    utils::read.csv(meta_data,
                    col.names=c('field_name', 'form_name', 'section_header',
                                'field_type', 'field_label', 'select_choices_or_calculations',
                                'field_note', 'text_validation_type_or_show_slider_number',
                                'text_validation_min', 'text_validation_max', 'identifier',
                                'branching_logic', 'required_field', 'custom_alignment',
                                'question_number', 'matrix_group_name', 'matrix_ranking',
                                'field_annotation'),
                    stringsAsFactors=FALSE)
  meta_data <- meta_data[meta_data$field_type != "descriptive", ]
  
  logic <- parseBranchingLogic(meta_data$branching_logic)
  names(logic) <- meta_data$field_name
  
  records <- .missingSummary_isMissingInField(records_orig, 
                                              meta_data, 
                                              logic)
  
  if (excludeMissingForms){
    records <- .missingSummary_excludeMissingForm(records, 
                                                  meta_data, 
                                                  logic)
  }
  
  .missingSummary_makeResultFrame(records, 
                                  meta_data)
}

# UNEXPORTED --------------------------------------------------------

.missingSummary_isMissingInField <- function(records_orig, 
                                             meta_data, 
                                             logic){
  records <- records_orig
  ignore_fld <- c(REDCAP_SYSTEM_FIELDS, meta_data$field_name[1])
  rec_name <- names(records)
  # Remove checkbox suffixes. This allows logic to be matched to the field.
  # defined in constants.R
  rec_name_cb <- sub(REGEX_CHECKBOX_FIELD_NAME, "\\1", rec_name, perl = TRUE)
  rec_name_clean <- sub("___[[:print:]]", "", rec_name)
  # get each field's metadata row number
  md_row <- match(rec_name_clean, meta_data$field_name)
  # identify checkbox groups
  is_cb <- !is.na(md_row) & meta_data[md_row, "field_type"] == "checkbox"
  cb_grp <- unique(rec_name_cb[is_cb])
  cb_grp_id <- match(rec_name_cb, cb_grp, nomatch = 0)
  cb_logic_grp <- vector('list', length(cb_grp))
  names(cb_logic_grp) <- cb_grp
  # get the name of the form on which the field is saved
  form_comp <- paste0(meta_data[md_row, "form_name"], "_complete")
  # is this always true? rec_name_cb == rec_name_clean

  for (i in seq_along(records)){
    # Actual field name
    this_field <- rec_name[i]
    # get the logic expression for this iteration of the loop
    this_logic <- logic[[rec_name_cb[i]]]
    tmp_form <- form_comp[i]

    # We are only going to look at fields that are informative as missing.
    # we skip fixed fields (see unexported) and the ID variable.
    if (!this_field %in% ignore_fld & !is.null(this_logic)) {
      # NOTE: in the result, TRUE means the value is missing
      #                      FALSE means the value is non-missing
      if (tmp_form == "NA_complete") {
        # I doubt these scenarios are possible
        # this_logic would be NULL and skipped
        if(grepl('_complete$', this_field)) {
          # If we are here, we are evaluating a `[form]_complete` field.
          # We just want to know if it is missing or not.
          records[[i]] <- is.na(records[[i]])
        } else {
          # If we are here, we did not find a matching form name. We will
          # assume variables not on a form are always non-missing.
          records[[i]] <- rep(FALSE, nrow(records))
        }
      } else if (!tmp_form %in% rec_name){
          # can this scenario occur?
          # no `[form]_complete` to evaluate, check missingness of value
          records[[i]] <- is.na(records[[i]])
      } else if (!is.expression(this_logic)) {
        # If we are here, there is not branching logic. 
        # If the `[form]_complete` field is missing, we return FALSE
        # If the `[form]_complete` is non-missing, we return the missingness of the value
        records[[i]] <- !is.na(records_orig[[tmp_form]]) & is.na(records_orig[[i]])
      } else {
        # Here we have branching logic.
        # If the `[form]_complete` field is missing, we return FALSE
        # If the `[form]_complete` is non-missing:
        #    The branching logic is satisfied: return the missingness of the value
        #    The branching logic is not satisfied: return FALSE
        #    The branching logic produces errors: return FALSE
        branch_eval <- tryCatch(eval(this_logic, records_orig), error = function(e) FALSE)
        ans <- !is.na(records_orig[[tmp_form]]) & branch_eval
        ans_ix <- which(ans)
        if(length(ans_ix) > 0L) {
          if(is_cb[i]) {
            # special case for checkbox group
            # examine all columns
            cb_grp_name <- cb_grp[cb_grp_id[i]]
            if(is.null(cb_logic_grp[[cb_grp_name]]$missing)) {
              cb_dat <- records_orig[ans_ix, cb_grp_id == cb_grp_id[i], drop = FALSE]
              # return TRUE if nothing is checked
              cb_no_checks <- rowSums(cb_dat != "0", na.rm = TRUE) == 0
              cb_logic_grp[[cb_grp_name]]$missing <- unname(cb_no_checks)
            }
            ans[ans_ix] <- cb_logic_grp[[cb_grp_name]]$missing
          } else {
            ans[ans_ix] <- is.na(records_orig[[i]][ans_ix])
          }
        }
        records[[i]] <- ans
      }
    }
  }
  # only post-logic checkbox groups will have length; remove others
  cb_logic_grp <- cb_logic_grp[lengths(cb_logic_grp) > 0L]
  # remove redundant records and rename
  if(length(cb_logic_grp) > 0L) {
    coi <- lapply(names(cb_logic_grp), function(i) which(match(i, cb_grp) == cb_grp_id))
    noi <- lapply(coi, function(i) rec_name[i])
    cnoi <- vapply(coi, function(i) rec_name_cb[i][1], character(1))
    c_rm <- unlist(lapply(coi, function(i) i[-1]))
    records <- records[,-c_rm]
    c_rename <- match(vapply(noi, `[`, character(1), 1), names(records))
    names(records)[c_rename] <- cnoi
  }
  records
}

.missingSummary_excludeMissingForm <- function(records, 
                                               meta_data, 
                                               logic){
  # Get the `[form]_complete` fields.
  form_names <- unique(meta_data$form_name)
  form_complete_names <- paste0(form_names, "_complete")
  form_complete_names <- form_complete_names[form_complete_names %in% names(records)]
  
  for (i in seq_len(nrow(records))){
    # For each record, find the fields associated with the forms
    # where the `[form]_complete` field is missing.
    is_this_form_complete_missing <- records[i, form_complete_names]
    is_this_form_complete_missing <- vapply(is_this_form_complete_missing, 
                                            FUN = is.na, 
                                            FUN.VALUE = logical(1))
    these_forms <- form_names[is_this_form_complete_missing]
    
    completeFormMissing <- lapply(these_forms,
                                  function(f){
                                    flds <- meta_data$field_name[meta_data$form_name %in% f]
                                    flds <- flds[!flds %in% meta_data$field_name[1]]
                                    flds <- flds[!flds %in% meta_data$field_name[meta_data$field_type == "checkbox"]]
                                    if (length(flds) == 0){
                                      return(NULL)
                                    } 
                                    else if (all(unlist(records[i, flds, drop = FALSE]) | sapply(logic[flds], is.expression))){
                                      return(flds)
                                    }
                                    else {
                                      return(NULL)
                                    }
                                  })
    # If the `[form]_complete` field is missing, we set the missingness value of the 
    # record for fields on that value to FALSE, indicating that they are non-missing
    # That is, we do not consider a value missing unless the form is marked either 'Complete' or 'Incomplete'
    completeFormMissing <- unlist(completeFormMissing)
    if (!is.null(completeFormMissing)){
      records[i, completeFormMissing] <- FALSE    
    } 
  }
  
  records
}

.missingSummary_makeResultFrame <- function(records, 
                                            meta_data){
  # These are the identifier fields in the result
  start_field <- c(meta_data$field_name[1], 
                   REDCAP_SYSTEM_FIELDS)
  start_field <- start_field[start_field %in% names(records)]
  
  # Make the initial data frame of results. Only the identifiers here
  MissingSummary <- records[start_field]
  
  # Remove the identifier fields from `records`.
  # This makes it easier to run an apply statement on the rows
  records <- records[!names(records) %in% start_field]
  
  # Number of missing values
  MissingSummary$n_missing <- numeric(nrow(records))
  MissingSummary$missing <- character(nrow(records))
  
  for (i in seq_len(nrow(MissingSummary))){
    missing_this_row <- vapply(records[i, ], 
                               FUN = isTRUE, 
                               FUN.VALUE = logical(1))
    MissingSummary$n_missing[i] <- sum(missing_this_row)
    MissingSummary$missing[i] <- paste0(names(records)[missing_this_row], 
                                        collapse = ", ")
  }
  
  MissingSummary
}

#' @name fieldCastingFunctions
#' @title Functions for Casting Fields After Export (Post Processing)
#'
#' @description The functions provided here allow for recasting fields
#'   after records have been exported. They generally have a similar
#'   interface to the casting strategy of [exportRecordsTyped()],
#'   though they may not each offer all the same options.
#'
#' @inheritParams common-rcon-arg
#' @inheritParams common-cast-args
#' @param data `data.frame` with the data fields to be recoded.
#' @param fields `character/logical/integerish`. A vector for identifying
#'   which fields to recode. When `logical`, the length must match
#'   the number of columns in `data` (i.e., recycling not permitted).
#'   A message is printed if any of the indicated fields are not a
#'   multiple choice field; no action will be taken on such fields.
#'   For this function, yes/no and true/false fields are considered
#'   multiple choice fields. Fields of class `mChoice` are quietly skipped.
#' @param drop_fields `character` or `NULL`. A vector of field names to remove from
#'   the data.
#' @param suffix `character(1)`. An optional suffix to provide if
#'   the recoded variables should be returned as new columns. For example,
#'   if recoding a field `forklift_brand` and `suffix = "_labeled"`,
#'   the result will have one column with the coded values
#'   (`forklift_brand`) and one column with the labeled values
#'   (`forklift_brand_labeled`).
#' @param quiet Print no messages if triggered, Default=FALSE.
#' @param threshold numeric(1). The threshold of non-NA data to trigger casting.
#' @param style character. One of "labelled" or "coded". Default is "labelled"
#' @param warn_zero_coded logical(1). Turn on or off warnings about zero coded fields. Default is TRUE.
#' @details `recastRecords` is a post-processing function motivated
#'   initially by the need to switch between codes and labels in multiple
#'   choice fields. Field types for which no casting function is specified will
#'   be returned with no changes. It will not attempt to validate the content
#'   of fields; fields that cannot be successfully cast will be quietly
#'   returned as missing values.
#'
#'   `castForImport` is written with defaults that will return data
#'   in a format ready to be imported to a project via `importRecords`.
#'   All fields are returned as character vectors. If any values fail to
#'   validation check, are report is returned as an attribute named `invalid`.
#'   This attribute may be retrieved using [reviewInvalidRecords()].
#'   These are then set to `NA`, which will be imported as blanks through
#'   the API.
#'
#'   `guessCast` is a helper function to make a guess at casting uncast
#'   columns. It will do a type cast if a validation is met above
#'   a threshold ratio of non-NA records. It modifies the existing
#'   `invalid` attribute to reflect the cast.
#'   This attribute may be retrieved using [reviewInvalidRecords()].
#'   `guessDate` is a special cast of `guessCast` that has defaults set for casting
#'   a date field.
#'
#'   `mChoiceCast` is a helper function that adds the `Hmisc::mChoice`
#'   multiple choice class. It adds a column for a multiple choice checkbox
#'   that is cast to the `Hmisc::mChoice` class. Requires `Hmisc`
#'   to be loaded.
#'
#' @inherit isZeroCodedCheckField sections
#'
#' @seealso
#' ## Exporting records
#'
#' [exportRecordsTyped()], \cr
#' [exportReportsTyped()], \cr
#' [fieldValidationAndCasting()], \cr
#' [reviewInvalidRecords()]
#'
#' ## Other Post Processing Functions
#'
#' [splitForms()], \cr
#' [widerRepeated()]
#'
#' ## Vignettes
#'
#' `vignette("redcapAPI-offline-connection", package = "redcapAPI")`\cr
#' `vignette("redcapAPI-casting-data")`\cr
#' `vignette("redcapAPI-missing-data-detection")`\cr
#' `vignette("redcapAPI-data-validation)`
#'
#'
#' @examples
#' \dontrun{
#' # Using recastRecords after export
#' Recs <-
#'   exportRecordsTyped(rcon) |>
#'   recastRecords(rcon,
#'                 fields = "dropdown_test",
#'                 cast = list(dropdown = castCode))
#'
#'
#' # Using castForImport
#' castForImport(Records,
#'               rcon)
#'
#'
#' # Using castForImport to recast zero-coded checkbox values
#' castForImport(Records,
#'               rcon,
#'               cast = list(checkbox = castCheckForImport(c("0", "Unchecked"))))
#'
#'
#' # Using guessCast
#' exportRecordsTyped(rcon,
#'                    validation=skip_validation,
#'                    cast = raw_cast) |>
#'   guessCast(rcon,
#'             validation=valRx("^[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])$"),
#'             cast=as.Date,
#'             threshold=0.6)
#'
#'
#' # Using mChoiceCast
#' exportRecordsTyped(rcon) |>
#'   mChoiceCast(rcon)
#'
#' }
#'
#'
#' @export

recastRecords <- function(data,
                          rcon,
                          fields,
                          cast = list(),
                          suffix    = "",
                          warn_zero_coded=TRUE){
  ###################################################################
  # Argument Validation #############################################
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_data_frame(x = data,
                               add = coll)

  checkmate::assert_class(x = rcon,
                          classes = "redcapConnection",
                          add = coll)

  checkmate::assert(
    checkmate::test_character(x = fields, null.ok = TRUE),
    checkmate::test_logical(x = fields, null.ok = TRUE),
    checkmate::test_integerish(x = fields, lower = 0, null.ok = TRUE),
    .var.name = "fields",
    add = coll
  )

  checkmate::assert_list(x = cast,
                         names = "named",
                         add = coll)

  checkmate::assert_character(x = suffix,
                              len = 1,
                              any.missing = FALSE,
                              add = coll)

  checkmate::assert_logical(x = warn_zero_coded,
                            len = 1,
                            any.missing = FALSE,
                            add = coll)

  checkmate::reportAssertions(coll)

  if (is.numeric(fields)){
    out_of_range <- fields[fields > ncol(data)]
    if (length(out_of_range) > 0){
      coll$push(sprintf("Columns {%s} requested in a data frame with %s columns",
                        paste0(out_of_range, collapse = ", "),
                        ncol(data)))
    }
  }

  if (is.logical(fields)){
    if (length(fields) != ncol(data)){
      coll$push(sprintf("'fields' (logical) should be of length %s and is length %s",
                        ncol(data),
                        length(fields)))
    }
  }

  checkmate::reportAssertions(coll)

  if (!is.character(fields)) fields <- names(data)[fields]

  checkmate::assert_subset(x = fields,
                           choices = names(data),
                           add = coll)

  checkmate::reportAssertions(coll)

  ###################################################################
  # Remove mChoice fields from processing                        ####

  is_mChoice <- vapply(data[fields],
                       FUN = inherits,
                       what = "mChoice",
                       FUN.VALUE = logical(1))
  fields <- fields[!is_mChoice]

  ###################################################################
  # Derive field information
  # Issue a warning if any of the fields are zero-coded check fields (See Issue 199)
  warnZeroCodedFieldPresent(fields, warn_zero_coded)

  MetaData <- rcon$metadata()

  field_names <- fields
  field_bases <- sub(REGEX_CHECKBOX_FIELD_NAME, #defined in constants.R
                     "\\1", field_names, perl = TRUE)
  field_text_types <- MetaData$text_validation_type_or_show_slider_number[match(field_bases, MetaData$field_name)]
  field_map <- match(field_bases, MetaData$field_name)

  field_types <- .castRecords_getFieldTypes(rcon = rcon,
                                            field_map = field_map,
                                            field_bases = field_bases,
                                            field_text_types = field_text_types)

  ###################################################################
  # Derive codings (This is probably a good internal helper)

  codings <- .castRecords_getCodings(rcon = rcon,
                                     field_map = field_map,
                                     field_names = field_names,
                                     field_types = field_types,
                                     code_check = TRUE)

  data <- .castRecords_recastRecords(Raw = data,
                                     cast = cast,
                                     field_types = field_types,
                                     codings = codings,
                                     field_names = field_names,
                                     suffix = suffix)

  data
}

#' @rdname fieldCastingFunctions
#' @export

castForImport <- function(data,
                          rcon,
                          fields     = NULL,
                          na         = list(),
                          validation = list(),
                          cast       = list(),
                          warn_zero_coded = TRUE){

  if (is.null(fields)) fields <- names(data)

  ###################################################################
  # Argument Valdiation                                          ####

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_data_frame(x = data,
                               col.names = "named",
                               add = coll)

  checkmate::assert_class(x = rcon,
                          classes = "redcapConnection",
                          add = coll)

  checkmate::assert(
    checkmate::test_character(x = fields, null.ok = TRUE),
    checkmate::test_logical(x = fields, null.ok = TRUE),
    checkmate::test_integerish(x = fields, lower = 0, null.ok = TRUE),
    .var.name = "fields",
    add = coll
  )

  checkmate::assert_list(x = na,
                         names = "named",
                         add = coll)

  checkmate::assert_list(x = validation,
                         names = "named",
                         null.ok= TRUE,
                         add = coll)

  checkmate::assert_list(x = cast,
                         names = "named",
                         add = coll)

  checkmate::reportAssertions(coll)

  .exportRecordsTyped_validateFieldForm(rcon = rcon,
                                        fields = fields,
                                        drop_fields = NULL,
                                        forms = NULL,
                                        coll = coll)

  checkmate::reportAssertions(coll)

  # Drop mChoice variables from frame
  mchoices <- vapply(data, inherits, logical(1), 'mChoice')
  if(sum(mchoices) > 0)
  {
    logMessage(paste0("The following mChoice variables(s) were dropped: ",
                   paste0(fields[mchoices], collapse=', '), '.'))
    data   <- data[,!mchoices, drop=FALSE]
    fields <- fields[!mchoices]
  }

  # Drop non importable field types
  for(type in c("file", "calc"))
  {
    drops <- rcon$metadata()[match(fields, rcon$metadata()$field_name),'field_type'] == type
    drops[is.na(drops)] <- FALSE
    if(sum(drops) > 0)
    {
      logMessage(paste0("The following ", type, " variables(s) were dropped: ",
                     paste0(fields[drops], collapse=', '), '.'))
      data   <- data[,!drops, drop=FALSE]
      fields <- fields[!drops]
    }
  }

  Raw <- as.data.frame(lapply(data,
                              function(x) trimws(as.character(x))))

  Recast <- .castRecords(Raw              = Raw[fields],
                         Records          = data[fields],
                         rcon             = rcon,
                         na               = na,
                         validation       = validation,
                         cast             = cast,
                         assignment       = NULL,
                         warn_zero_coded  = warn_zero_coded,
                         default_cast     = .default_cast_import,
                         default_validate = .default_validate_import)

  for (i in fields){
    data[[i]] <- Recast[[i]]
  }

  attr(data, "invalid") <- attr(Recast, "invalid")
  attr(data, "castForImport") <- TRUE

  data
}

#' @rdname fieldCastingFunctions
#' @export

guessCast <- function(data,
                      rcon,
                      na         = isNAorBlank,
                      validation,
                      cast,
                      quiet      = FALSE,
                      threshold  = 0.8)
{
  ##########################################
  ## Validate Arguments
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x       = data,
                          classes = "data.frame",
                          add     = coll)

  checkmate::assert_class(x       = rcon,
                          classes = "redcapApiConnection",
                          add     = coll)

  checkmate::assert_function(x   = na,
                             add = coll)

  checkmate::assert_function(x   = validation,
                             add = coll)

  checkmate::assert_function(x   = cast,
                             add = coll)

  checkmate::assert_logical(x           = quiet,
                            len         = 1,
                            any.missing = FALSE,
                            add         = coll)

  checkmate::assert_numeric(x           = threshold,
                            len         = 1,
                            any.missing = FALSE,
                            add         = coll)

  checkmate::reportAssertions(coll)

  ##########################################
  ## Loop over text columns to guess on
  field_classes <- sapply(data, class)
  text_fields   <- names(data)[field_classes == "character"]
  for(i in text_fields)
  {
    nas   <- isNAorBlank(data[[i]])
    valid <- validation(data[[i]])
    sel   <- !valid & !nas
    if ( any(!nas) && sum(valid)/sum(!nas) >= threshold )
    {
      if(!quiet)
        logMessage(paste0("guessCast triggered on ", i,
                       " for ", sum(valid),
                       " of ", length(valid), " records."))
      x <- data[[i]]

      # Modify "invalid" attribute if needed.
      if(any(sel))
      {
        inv <- attr(data, "invalid")

        inv <- rbind(
          inv,
          data.frame(row        = seq_len(nrow(data))[sel],
                     record_id  = NA,
                     field_name = i,
                     field_type = "text",
                     value      = data[sel, i])
        )
        attr(data, "invalid") <- inv
      }

      x[ nas | !valid ] <- NA
      data[[i]] <- cast(x)
      for(j in names(attributes(x))) attr(data[[i]], j) <- attr(x, j)
    }
  }

  if(!is.null(attr(data, "invalid")))
  {
    class(attr(data, "invalid")) <- c("invalid", "data.frame")
    attr(attr(data, "invalid"), "time")    <- format(Sys.Date(), "%c")
    attr(attr(data, "invalid"), "version") <- rcon$version()
    attr(attr(data, "invalid"), "project") <- rcon$projectInfo()$project_title
  }

  data
}

#' @rdname fieldCastingFunctions
#' @export

guessDate <- function(data,
                      rcon,
                      na         = isNAorBlank,
                      validation = valRx("^[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])$"),
                      cast       = function(x, ...) as.POSIXct(x, format = "%Y-%m-%d"),
                      quiet      = FALSE,
                      threshold  = 0.8){
  guessCast(data,
            rcon       = rcon,
            na         = na,
            validation = validation,
            cast       = cast,
            quiet      = quiet,
            threshold  = threshold)
}

#' @rdname fieldCastingFunctions
#' @export

mChoiceCast <- function(data,
                        rcon,
                        style = "labelled",
                        drop_fields = TRUE)
{
  ###################################################################
  # Check arguments
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x       = data,
                          classes = "data.frame",
                          add     = coll)

  checkmate::assert_class(x       = rcon,
                          classes = "redcapApiConnection",
                          add     = coll)

  checkmate::assert_logical(x     = drop_fields,
                           len    = 1)

  style <- checkmate::matchArg(x = style,
                               choices = c("coded", "labelled"),
                               add = coll)

  checkmate::reportAssertions(coll)

  if(!"package:Hmisc" %in% search())
    logWarning("Hmisc is not loaded. Required to use an mChoice class.")

  FieldNames <- rcon$fieldnames()
  export_fields <- FieldNames$export_field_name[FieldNames$export_field_name %in% colnames(data)]
  fields <- FieldNames$original_field_name[FieldNames$export_field_name %in% colnames(data)]
  fields <- unique(fields)

  MetaData <- rcon$metadata()
  CheckboxMetaData <- MetaData[MetaData$field_type == "checkbox", ]

  checkbox_fields <- fields[fields %in% CheckboxMetaData$field_name]

  Raw <- recastRecords(data, rcon, fields = export_fields, cast=list(checkbox=castRaw))
  for (i in checkbox_fields)
    data[[ i ]] <-
    .mChoiceField(rcon,
                  records_raw = Raw,
                  checkbox_fieldname = i,
                  style = style)

  # get the suffixed field names
  fields_to_drop <- character()
  for (i in checkbox_fields) {
    fields <- FieldNames$export_field_name[FieldNames$original_field_name %in% i]
    fields_to_drop <- c(fields_to_drop, fields)
  }

  # if drop_fields is FALSE, keep suffixed field, else if drop_fields is TRUE (default) remove suffixed field
  if (drop_fields)
  {
    attrs <- names(attributes(data))[!names(attributes(data)) %in% c("names", "class", "row.names")]
    preserve <- lapply(attrs, function(x) attr(data, x))
    names(preserve) <- attrs

    data <- data[, !names(data) %in% fields_to_drop]
    for(i in attrs) attr(data, i) <- preserve[[i]]
  }

  data
}

#####################################################################
# Unexported                                                     ####

.castRecords <- function(Raw,
                         Records          = NULL,
                         rcon,
                         na               = NULL,
                         validation       = NULL,
                         cast             = NULL,
                         assignment       = NULL,
                         default_cast     = .default_cast,
                         default_validate = .default_validate,
                         batch_size       = NULL,
                         warn_zero_coded  = TRUE){
  # batch_size will be passed to rcon$externalCoding() if batch_size was passed to the executing function
  #                              This occurs in .castRecords_getCodings
  # Issue a warning if any of the fields are zero-coded check fields (See Issue 199)
  warnZeroCodedFieldPresent(names(Raw), warn_zero_coded)

  ###################################################################
  # Process meta data for useful information                     ####

  ###################################################################
  # Derive field information                                     ####
  MetaData <- rcon$metadata()

  field_names <- names(Raw)

  field_bases <- sub(REGEX_CHECKBOX_FIELD_NAME, #defined in constants.R
                     "\\1", field_names, perl = TRUE)
  field_text_types <- MetaData$text_validation_type_or_show_slider_number[match(field_bases, MetaData$field_name)]
  field_map <- match(field_bases, MetaData$field_name)

  field_types <- .castRecords_getFieldTypes(rcon             = rcon,
                                            field_map        = field_map,
                                            field_bases      = field_bases,
                                            field_text_types = field_text_types)

  ###################################################################
  # Derive codings                                               ####
  codings <- .castRecords_getCodings(rcon        = rcon,
                                     field_map   = field_map,
                                     field_names = field_names,
                                     field_types = field_types,
                                     code_check  = TRUE,
                                     batch_size  = batch_size)

  ###################################################################
  # Common provided args for na / validate functions             ####
  args <- lapply(seq_along(Raw),
                 function(x) list(x          = Raw[[x]],
                                  field_name = field_names[x],
                                  coding     = codings[[x]]))

  ###################################################################
  # Locate NA's                                                  ####
  nas <- .castRecords_getNas(na             = na,
                             field_types    = field_types,
                             args           = args,
                             correct_length = nrow(Raw))

  ###################################################################
  # Run Validation Functions

  if(is.null(validation)) validation <- skip_validation

  validations <-
    .castRecords_runValidation(Raw              = Raw,
                               validation       = validation,
                               field_types      = field_types,
                               args             = args,
                               correct_length   = nrow(Raw),
                               default_validate = default_validate)

  ###################################################################
  # Type Casting                                                 ####

  Records <-
    .castRecords_castRecords(Raw          = if (is.null(Records)) Raw else Records,
                             cast         = cast,
                             field_types  = field_types,
                             nas          = nas,
                             validations  = validations,
                             codings      = codings,
                             field_names  = field_names,
                             default_cast = default_cast)

  ###################################################################
  # Handle Attributes assignments on columns,                    ####
  Records <- .castRecords_attributeAssignment(Records     = Records,
                                              assignment  = assignment,
                                              field_names = field_names,
                                              MetaData    = MetaData,
                                              field_map   = field_map)

  ###################################################################
  # Attach invalid record information                            ####

  Records <- .castRecords_attachInvalid(rcon        = rcon,
                                        Records     = Records,
                                        Raw         = Raw,
                                        validations = validations,
                                        nas         = nas,
                                        field_names = field_names,
                                        field_types = field_types)

  ###################################################################
  # Return Results                                               ####
  Records
}

# .exportRecordsTyped_getFieldTypes ---------------------------------

.castRecords_getFieldTypes <- function(rcon,
                                       field_map,
                                       field_bases,
                                       field_text_types){

  field_types <- rcon$metadata()$field_type[field_map]

  # only select form_name with _complete, avoid fields that end in _complete
  form_complete_names <- paste0(unique(rcon$metadata()$form_name), "_complete")
  field_types[field_bases %in% form_complete_names] <- "form_complete"

  choices <- rcon$metadata()$select_choices_or_calculations[field_map]

  # autocomplete was added to the text_validation... column for
  # dropdown menus with the autocomplete feature.
  field_types[field_types == "text" & !is.na(field_text_types)] <-
    field_text_types[field_types == "text" & !is.na(field_text_types)]

  field_types <- gsub("_(dmy|mdy|ymd)$", "_", field_types)
  field_types[is.na(field_types)] <- "text"

  # Designate events and DAGs as system fields
  which_system_field <- which(field_bases %in% c("redcap_event_name",
                                                 "redcap_repeat_instrument",
                                                 "redcap_data_access_group"))
  field_types[which_system_field] <- rep("system", length(which_system_field))

  # Set system fields to text if the data sets needed to make the code books
  # is not present. (this is more common with offline connections)
  if (is.null(rcon$events())){
    field_types[field_bases %in% "redcap_event_name"] <-
      rep("text",
          sum(field_bases %in% "redcap_event_name"))
  }

  if (is.null(rcon$events())){
    field_types[field_bases %in% "redcap_data_access_group"] <-
      rep("text",
          sum(field_bases %in% "redcap_data_access_group"))
  }

  field_types[field_types == "text" &
                grepl("BIOPORTAL", choices, ignore.case = TRUE)] <- "bioportal"

  field_types
}

# .exportRecordsTyped_getCodings ------------------------------------
.castRecords_getCodings <- function(rcon = rcon,
                                    field_map = field_map,
                                    field_names = field_names,
                                    field_types = field_types,
                                    code_check = FALSE,
                                    batch_size = NULL){
  # code_check is not needed in exportRecordsTyped
  # in recastData, however, we need a codebook for checkboxes
  codebook <- rcon$metadata()$select_choices_or_calculations[field_map]
  codebook[! field_types %in% c("select", "radio", "dropdown", if (code_check) "checkbox" else character(0))] <- NA
  codebook[field_types == "form_complete"] <- "0, Incomplete | 1, Unverified | 2, Complete"
  codebook[field_types == "yesno"] <- "0, No | 1, Yes"


  system_field <- which(field_names %in% c("redcap_event_name",
                                           "redcap_data_access_group",
                                           "redcap_repeat_instrument"))
  codebook[system_field] <- vapply(field_names[system_field],
                                   FUN = .castRecords_getSystemCoding,
                                   FUN.VALUE = character(1),
                                   rcon = rcon)

  codings <- vector("list", length = length(codebook))

  for (i in seq_along(codings)){
    codings[[i]] <-
      if (is.na(codebook[i])){
        if (field_types[i] %in% c("bioportal", "sql")){
          ext_code <- rcon$externalCoding(batch_size = batch_size)[[field_names[i] ]]
          if (is.null(ext_code)){
            ext_code <- NA_character_
          }
          ext_code
        } else {
          NA_character_
        }
      } else {
        this_mapping <- fieldChoiceMapping(object = codebook[i],
                                           field_name = field_names[i])
        this_coding <- this_mapping[, 1]
        names(this_coding) <- this_mapping[, 2]
        this_coding
      }
  }

  codings
}

# This function gets the codings for system fields so that they can be
# cast to raw or label. These apply to
# redcap_event_name (requires both rcon$arms and rcon$events)
# redcap_repeat_instrument (requires rcon$instruments)
# redcap_data_access_group (requires rcon$dags)
.castRecords_getSystemCoding <- function(field_name,
                                         rcon){
  if (field_name == "redcap_event_name"){
    Event <- rcon$events()
    Arm <- rcon$arms()

    if (!is.null(Event) && !is.null(Arm)){
      EventArm <- merge(Event,
                        Arm,
                        by = "arm_num",
                        all.x = TRUE)

      EventArm$data_label <- sprintf("%s (Arm %s: %s)",
                                     EventArm$event_name,
                                     EventArm$arm_num,
                                     EventArm$name)

      Mapping <- data.frame(code = EventArm$unique_event_name,
                            label = EventArm$data_label,
                            stringsAsFactors = FALSE)
    } else {
      return(NA_character_)
    }

  } else if (field_name == "redcap_data_access_group") {
    Dag <- rcon$dags()

    if (is.null(Dag)) return(NA_character_)

    Mapping <- data.frame(code = Dag$unique_group_name,
                          label = Dag$data_access_group_name,
                          stringsAsFactors = FALSE)
  } else if (field_name == "redcap_repeat_instrument"){
    Instrument <- rcon$instruments()
    Mapping <- data.frame(code = Instrument$instrument_name,
                          label = Instrument$instrument_label,
                          stringsAsFactors = FALSE)
  } else {
    return(NA_character_)
  }


  coding <- sprintf("%s, %s",
                    Mapping$code,
                    Mapping$label)
  coding <- paste0(coding, collapse = " | ")

  coding
}

# .exportRecords_getNas ---------------------------------------------
.castRecords_getNas <- function(na,
                                field_types,
                                args,
                                correct_length){
  funs <- lapply(field_types, function(x) if(is.null(na[[x]])) isNAorBlank else na[[x]])
  nas  <- mapply(do.call, funs, args, SIMPLIFY = FALSE)

  is_correct_length <- vapply(nas, function(x) length(x) == correct_length, logical(1))
  is_logical <- vapply(nas, is.logical, logical(1))

  cm <- checkmate::makeAssertCollection()

  if (any(!is_correct_length & is_logical)){
    m <- unique(field_types[!is_correct_length & !is_logical])
    cm$push(paste("User supplied na method for [",
                  paste(m, collapse=", "),
                  "] not returning vector of logical of correct length"))
  }

  if (any(!is_logical)){
    m <- unique(field_types[!is_logical])
    cm$push(paste("User supplied na method for [",
                  paste(m, collapse=", "),
                  "] must return a logical vector"))
  }

  checkmate::reportAssertions(cm)

  matrix(unlist(nas), ncol = length(nas), byrow = FALSE)
}

# .exportRecordsTyped_runValidation ---------------------------------
.castRecords_runValidation <- function(Raw,
                                       validation,
                                       field_types,
                                       args,
                                       correct_length,
                                       default_validate = .default_validate){
  validate <- utils::modifyList(default_validate, validation)

  funs <- lapply(
    field_types,
    function(x)
    {
      f <- validate[[x]]
      # No validate function is an auto pass
      if(is.null(f)) function(...) rep(TRUE,nrow(Raw)) else f
    })

  validations <- mapply(do.call, funs, args, SIMPLIFY = FALSE)

  is_correct_length <- vapply(validations, function(x) length(x) == correct_length, logical(1))
  is_logical <- vapply(validations, is.logical, logical(1))

  cm <- checkmate::makeAssertCollection()

  if (any(!is_correct_length & is_logical)){
    m <- unique(field_types[!is_correct_length & !is_logical])
    cm$push(paste("User supplied validation method for [",
                  paste(m, collapse=", "),
                  "] not returning vector of correct length logical"))
  }

  if (any(!is_logical)){
    m <- unique(field_types[!is_logical])
    cm$push(paste("User supplied validation method for [",
                  paste(m, collapse=", "),
                  "] must return a logical vectors"))
  }

  checkmate::reportAssertions(cm)

  matrix(unlist(validations), ncol = length(validations), byrow = FALSE)
}

# .exportRecordsTyped_castRecords -----------------------------------
# We provide 'castRecords' and 'recastRecords' options. The first
# is needed for exportRecordsTyped, and the second for recastData.
# They are very similar in concept, and changes to one may indicate
# changes to the other. For this reason, we want to keep them close to
# each other to remind us to review both of them if either requires an edit

.castRecords_castRecords <- function(Raw,
                                     cast,
                                     field_types,
                                     nas,
                                     validations,
                                     codings,
                                     field_names,
                                     default_cast = .default_cast){
  # REMINDER: Any changes to this method may suggest changes are needed to .exportRecordsTyped_recastRecords
  Records <- Raw
  cast <- utils::modifyList(default_cast, cast)
  # Edits to this for loop may necessitate edits to the for loop in recastData
  for(i in seq_along(Raw))
  {
    if(field_types[i] %in% names(cast))
    {
      x <- Raw[[i]]
      x[ nas[,i] | !validations[,i] ] <- NA
      typecast <- cast[[ field_types[i] ]]

      if(is.function(typecast))
        Records[[i]] <- typecast(x, field_name=field_names[i], coding=codings[[i]])
    }
  }
  names(Records) <- names(Raw)

  Records
}

.castRecords_recastRecords <- function(Raw,
                                       cast,
                                       field_types,
                                       codings,
                                       field_names,
                                       suffix){
  # REMINDER: Any changes to this method may suggest changes are needed to .exportRecordsTyped_castRecords
  Records <- Raw
  for(i in seq_along(field_names))
  {
    if(field_types[i] %in% names(cast))
    {
      # generate the new field name
      this_field_name <- sprintf("%s%s",
                                 field_names[i],
                                 suffix)

      x <- Records[[ field_names[i] ]]

      # preserve the attributes on the field (but dropping class and factor levels)
      this_attribute <- attributes(x)
      this_attribute <- this_attribute[!names(this_attribute) %in% c("class", "levels")]

      typecast <- cast[[ field_types[i] ]]
      if(is.function(typecast)){
        Records[[ this_field_name ]] <- typecast(x, field_name=field_names[i], coding=codings[[i]])
        # reapply the attributes
        attributes(Records[[ this_field_name ]]) <- c(attributes(Records[[ this_field_name ]]),
                                                      this_attribute)
      }
    }
  }

  Records
}

# .exportRecordsTyped_attributeAssignment ---------------------------
.castRecords_attributeAssignment <- function(Records,
                                             assignment,
                                             field_names,
                                             MetaData,
                                             field_map){
  # Construct the field labels while accomodating checkbox field choices
  field_labels <- mapply(.castRecords_makeFieldLabel,
                         field_names,
                         field_map,
                         MoreArgs = list(MetaData = MetaData),
                         SIMPLIFY = TRUE,
                         USE.NAMES = FALSE)

  for(i in names(assignment))
  {
    x <- assignment[[i]](field_names, field_labels, MetaData$field_annotation[field_map])

    for(j in seq_along(Records)) if(!is.na(x[j])) attr(Records[,j], i) <- x[j]
  }
  Records
}

.castRecords_makeFieldLabel <- function(field_name, field_map, MetaData){
  is_checkbox <- MetaData$field_type[field_map] %in% "checkbox"

  # checkbox field labels need to follow the pattern '[field_label] (choice=[choice_label])'
  # Issue #301
  if (is_checkbox){
    check_choice <- sub(REGEX_CHECKBOX_FIELD_NAME, "\\2", field_name, perl = TRUE)
    Mapping <- fieldChoiceMapping(MetaData$select_choices_or_calculations[field_map])
    Mapping <- Mapping[which(Mapping[, 1] == check_choice), ]
    sprintf("%s (choice=%s)",
            MetaData$field_label[field_map],
            Mapping[2])
  } else {
    MetaData$field_label[field_map]
  }
}

# .exportRecordsTyped_attachInvalid ---------------------------------
.castRecords_attachInvalid <- function(rcon,
                                       Records,
                                       Raw,
                                       validations,
                                       nas,
                                       field_names,
                                       field_types){
  selector <- !validations & !nas

  id_field <- rcon$metadata()$field_name[1]

  MD <- rcon$metadata()
  base_names <- sub(REGEX_CHECKBOX_FIELD_NAME, "\\1", field_names, perl = TRUE)
  form_names <- MD$form_name[match(base_names, MD$field_name)]

  event_id <-
    if ("redcap_event_name" %in% names(Raw)){
      event_index <- match(Raw$redcap_event_name,
                           rcon$events()$unique_event_name)
      rcon$events()$event_id[event_index]
    } else {
      rep(NA_real_, nrow(Raw))
    }

  Invalid <-
    do.call(rbind,
            lapply(seq_along(Raw),
                   function(i)
                   {
                     sel <- selector[,i]
                     if (any(sel))
                     {
                       df <- data.frame(row = seq_len(nrow(Raw))[sel],
                                  record_id = if(id_field %in% colnames(Raw)) Raw[sel, id_field] else NA,
                                  field_name = field_names[i],
                                  form_name = form_names[i],
                                  field_type = field_types[i],
                                  event_id = if (is.null(event_id)) NA_character_ else event_id[sel],
                                  value = Raw[sel, i],
                                  stringsAsFactors = FALSE)
                       df
                     } else NULL
                   }
            )
    )

  if (is.null(Invalid)){
    Invalid <- data.frame(row = numeric(0),
                          record_id = character(0),
                          field_name = character(0),
                          form_name = character(0),
                          field_type = character(0),
                          event_id = numeric(0),
                          value = character(0),
                          link_to_form = character(0),
                          stringsAsFactors = FALSE)
  } else {
    Invalid$link_to_form <-
      constructLinkToRedcapForm(rcon,
                                form_name = Invalid$form_name,
                                record_id = Invalid$record_id,
                                event_id = Invalid$event_id)
  }



  class(Invalid) <- c("invalid", "data.frame")
  attr(Invalid, "time") <- format(Sys.Date(), "%c")
  attr(Invalid, "version") <- rcon$version()
  attr(Invalid, "project") <- rcon$projectInfo()$project_title

  if (nrow(Invalid) > 0)
  {
    logWarning("Some records failed validation. Use `reviewInvalidRecords` to review the failures.")
  }

  attr(Records, "invalid") <- Invalid

  Records
}

# mChoice Function --------------------------------------------------
.mChoiceField <- function(rcon,
                          records_raw,
                          checkbox_fieldname,
                          style = c("coded", "labelled")){

  ##################################################################
  # Argument Validation

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)

  checkmate::assert_data_frame(records_raw,
                               add = coll)

  checkmate::assert_character(x = checkbox_fieldname,
                              len = 1,
                              any.missing = FALSE,
                              add = coll)

  style <- checkmate::matchArg(x = style,
                               choices = c("coded", "labelled"),
                               add = coll)

  checkmate::reportAssertions(coll)

  FieldNames <- rcon$fieldnames()

  checkmate::assert_subset(x = checkbox_fieldname,
                           choices = FieldNames$original_field_name,
                           add = coll)

  checkmate::reportAssertions(coll)

  MetaData <- rcon$metadata()

  field_type <- MetaData$field_type[MetaData$field_name == checkbox_fieldname]

  if (field_type != "checkbox")
  {
    coll$push(sprintf("'%s' is not a checkbox field; it cannot be made into an mChoice field",
                      checkbox_fieldname))

    checkmate::reportAssertions(coll)
  }

  ##################################################################
  # Make the mChoice field

  # get the suffixed field names
  fields <- FieldNames$export_field_name[FieldNames$original_field_name %in% checkbox_fieldname]

  if (length(fields) == 0) return(NULL)

  # get the options
  opts   <- fieldChoiceMapping(rcon, checkbox_fieldname)
  levels <- opts[, 1+(style == "labelled"), drop = TRUE]

  # Make the data frame to store the status of the options
  opts <- as.data.frame(matrix(rep(seq_along(fields), nrow(records_raw)), nrow=nrow(records_raw), byrow=TRUE))
  checked <- records_raw[,fields] != '1' # Logical value indicating if the choice was checked
  opts[which(checked,arr.ind=TRUE)] <- "" # Make unchecked choices an empty string

  # Consolidate choices into the mChoice object
  structure(
    gsub(";$|^;", "",gsub(";{2,}",";", do.call('paste', c(opts, sep=";")))),
    label  = checkbox_fieldname,
    levels = levels,
    class  = c("mChoice", "labelled"))
}

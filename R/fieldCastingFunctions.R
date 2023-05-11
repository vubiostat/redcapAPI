#' @name fieldCastingFunctions
#' @title Functions for Casting Fields After Export (Post Processing)
#' 
#' @description The functions provided here allow for recasting fields 
#'   after records have been exported. They generally have a similar 
#'   interface to the casting strategy of \code{\link{exportRecordsTyped}}, 
#'   though they may not each offer all the same options. 
#'   
#' @param data \code{data.frame} with the data fields to be recoded. 
#' @param rcon \code{recapConnection} object.
#' @param fields \code{character/logical/integerish}. A vector for identifying
#'   which fields to recode. When \code{logical}, the length must match 
#'   the number of columns in \code{data} (i.e., recycling not permitted).
#'   A message is printed if any of the indicated fields are not a 
#'   multiple choice field; no action will be taken on such fields.
#'   For this function, yes/no and true/false fields are considered 
#'   multiple choice fields. Fields of class \code{mChoice} are quietly skipped.
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
#' @param cast A named \code{list} of user specified class casting functions. 
#'   Keys must correspond to a truncated REDCap field type, i.e.
#'   {date_, datetime_, datetime_seconds_, time_mm_ss, time_hh_mm_ss, time, float,
#'   number, calc, int, integer, select, radio, dropdown, yesno, truefalse,
#'   checkbox, form_complete, sql}. The function will be 
#'   provided the variables (x, field_name, coding). 
#'   See \code{\link{fieldValidationAndCasting}}
#' @param suffix \code{character(1)}. An optional suffix to provide if 
#'   the recoded variables should be returned as new columns. For example, 
#'   if recoding a field \code{forklift_brand} and \code{suffix = "_labelled"}, 
#'   the result will have one column with the coded values 
#'   (\code{forklift_brand}) and one column with the labelled values 
#'   (\code{forklift_brand_labelled}).
#' @param quiet Print no messages if triggered, Default=FALSE. 
#' @param threshold numeric(1). The threshold of non-NA data to trigger casting.
#' @param style character. One of "labelled" or "coded". Default is "labelled"
#'   
#' @details \code{recastRecords} is a post-processing function motivated 
#'   initially by the need to switch between codes and labels in multiple 
#'   choice fields. Field types for which no casting function is specified will
#'   be returned with no changes. It will not attempt to validate the content
#'   of fields; fields that can not be successfully cast will be quietly 
#'   returned as missing values. 
#'   
#'   \code{castForImport} is written with defaults that will return data 
#'   in a format ready to be imported to a project via \code{importRecords}. 
#'   All fields are returned as character vectors. If any values fail to
#'   validation check, are report is returned as an attribute named \code{invalid}. 
#'   These are then set to \code{NA}, which will be imported as blanks through
#'   the API. 
#'   
#'   \code{guessCast} is a helper function to make a guess at casting uncast 
#'   columns. It will do a type cast if a validation is met above
#'   a threshold ratio of non-NA records. It modifies the existing
#'   \code{invalid} attribute to reflect the cast. \code{guessDate} is 
#'   a special cast of \code{guessCast} that has defaults set for casting
#'   a date field.
#'   
#'   \code{mChoiceCast} is a helper function that adds the \code{Hmisc::mChoice} 
#'   multiple choice class. It adds a column for a multiple choice checkbox 
#'   that is cast to the \code{Hmisc::mChoice} class. Requires \code{Hmisc} 
#'   to be loaded.
#'   
#'   
#' @examples
#' \dontrun{
#' # Using guessCast
#' recs <- exportRecordsTyped(rcon, 
#'                            cast = raw_cast) |> 
#'   guessCast(rcon, 
#'             validation=valRx("^[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])$"), 
#'             cast=as.Date,
#'             threshold=0.6)
#' 
#' # Using mChoiceCast            
#' recs <- exportRecordsTyped(rcon) |> mChoiceCast(rcon)
#' }

#' @rdname fieldCastingFunctions
#' @export

recastRecords <- function(data, 
                          rcon, 
                          fields, 
                          cast = list(), 
                          suffix    = ""){
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
                            cast       = list()){
  
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
  
  Raw <- as.data.frame(lapply(data, 
                              function(x) trimws(as.character(x))))
  
  Recast <- .castRecords(Raw              = Raw[fields], 
                         Records          = data[fields], 
                         rcon             = rcon, 
                         na               = na, 
                         validation       = validation, 
                         cast             = cast, 
                         assignment       = NULL, 
                         default_cast     = .default_cast_import, 
                         default_validate = .default_validate_import)
  
  for (i in fields){
    data[[i]] <- Recast[[i]]
  }
  
  attr(data, "invalid") <- attr(Recast, "invalid")
  
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
        message(paste0("guessCast triggered on ", i,
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
                        style = "labelled")
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
  
  style <- checkmate::matchArg(x = style, 
                               choices = c("coded", "labelled"), 
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  if(!"package:Hmisc" %in% search())
    warning("Hmisc is not loaded. Required to use an mChoice class.")
  
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
                         default_validate = .default_validate){
  
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
                                     code_check  = TRUE)
  
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
  # Run Validation Functions                                     ####
  
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
  field_types[grepl("_complete$", field_bases)] <- "form_complete"
  
  # autocomplete was added to the text_validation... column for
  # dropdown menus with the autocomplete feature.
  field_types[field_types == "text" & !is.na(field_text_types)] <-
    field_text_types[field_types == "text" & !is.na(field_text_types)]
  
  field_types <- gsub("_(dmy|mdy|ymd)$", "_", field_types)
  field_types[is.na(field_types)] <- "text"
  
  field_types
}

# .exportRecordsTyped_getCodings ------------------------------------
.castRecords_getCodings <- function(rcon = rcon, 
                                    field_map = field_map, 
                                    field_names = field_names, 
                                    field_types = field_types, 
                                    code_check = FALSE){
  # code_check is not needed in exportRecordsTyped
  # in recastData, however, we need a codebook for checkboxes
  codebook <- rcon$metadata()$select_choices_or_calculations[field_map]
  codebook[! field_types %in% c("select", "radio", "dropdown", if (code_check) "checkbox" else character(0))] <- NA
  codebook[field_types == "form_complete"] <- "0, Incomplete | 1, Unverified | 2, Complete"
  codebook[field_types == "yesno"] <- "0, No | 1, Yes"
  
  codings <- vector("list", length = length(codebook))
  
  for (i in seq_along(codings)){
    codings[[i]] <-
      if (is.na(codebook[i])){
        NA_character_
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
  validate <- modifyList(default_validate, validation)
  
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
  cast <- modifyList(default_cast, cast)
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
  for(i in names(assignment))
  {
    x <- assignment[[i]](field_names, MetaData$field_label[field_map], MetaData$field_annotation[field_map])
    for(j in seq_along(Records)) if(!is.na(x[j])) attr(Records[,j], i) <- x[j]
  }
  Records
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
  
  attr(Records, "invalid") <-
    do.call(rbind, lapply(seq_along(Raw), function(i)
    {
      sel <- selector[,i]
      if(any(sel))
      {
        data.frame(row=seq_len(nrow(Raw))[sel],
                   record_id=if(id_field %in% colnames(Raw)) Raw[sel, id_field] else NA,
                   field_name=field_names[i],
                   field_type=field_types[i],
                   value=Raw[sel, i])
      } else NULL
    }))
  if(!is.null(attr(Records, "invalid")))
  {
    class(attr(Records, "invalid")) <- c("invalid", "data.frame")
    attr(attr(Records, "invalid"), "time") <- format(Sys.Date(), "%c")  
    attr(attr(Records, "invalid"), "version") <- rcon$version()
    attr(attr(Records, "invalid"), "project") <- rcon$projectInfo()$project_title
    warning("Some records failed validation. See 'invalid' attr.")
  }
  
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

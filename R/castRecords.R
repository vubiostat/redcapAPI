castRecords <- function(Raw, 
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


#####################################################################
# Unexported                                                     ####
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

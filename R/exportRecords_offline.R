#' @rdname exportRecords
#' @export

exportRecords_offline <- function(dataFile, 
                                  metaDataFile, 
                                  factors = TRUE, 
                                  fields = NULL,
                                  forms=NULL, 
                                  labels = TRUE,
                                  dates = TRUE, 
                                  checkboxLabels = FALSE, 
                                  colClasses = NA,
                                  ..., 
                                  meta_data)
{
  if (!missing(meta_data)){
    warning("Argument `meta_data` has been deprecated. Please use `metaDataFile` instead.")
    if (missing(metaDataFile)){
      metaDataFile <- meta_data
    }
  }
  
   ##################################################################
  # Argument Validation 
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = dataFile, 
                              len = 1, 
                              any.missing = FALSE, 
                              add = coll)
  
  checkmate::assert_character(x = metaDataFile, 
                              len = 1, 
                              any.missing = FALSE, 
                              add = coll)
  
  checkmate::assert_logical(x = factors, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_character(x = fields, 
                              any.missing = FALSE, 
                              null.ok = TRUE, 
                              add = coll)
  
  checkmate::assert_character(x = forms, 
                              any.missing = FALSE, 
                              null.ok = TRUE, 
                              add = coll)
  
  checkmate::assert_logical(x = labels, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = dates, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = checkboxLabels, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  if (is.list(colClasses)) colClasses <- unlist(colClasses)
  
  checkmate::assert_character(x = colClasses, 
                              names = "named", 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
   ##################################################################
  # Prepare the Meta Data
  MetaData <- utils::read.csv(metaDataFile,
                              stringsAsFactors = FALSE,
                              na.strings = "")
  
  col.names=c('field_name', 'form_name', 'section_header', 
              'field_type', 'field_label', 'select_choices_or_calculations', 
              'field_note', 'text_validation_type_or_show_slider_number', 
              'text_validation_min', 'text_validation_max', 'identifier', 
              'branching_logic', 'required_field', 'custom_alignment', 
              'question_number', 'matrix_group_name', 'matrix_ranking',
              'field_annotation')
  
  names(MetaData) <- col.names[1:length(col.names)]
  
  # for purposes of the export, we don't need the descriptive fields. 
  # Including them makes the process more error prone, so we'll ignore them.
  MetaData <- MetaData[!MetaData$field_type %in% "descriptive", ]  
  
  # Check that all fields exist in the meta data
  if (!is.null(fields)) 
  {
    bad_fields <- fields[!fields %in% MetaData$field_name]
    if (length(bad_fields))
      coll$push(paste0("The following are not valid field names: ",
                       paste0(bad_fields, collapse = ", ")))
  }
  
  # Check that all form names exist in the meta data
  if (!is.null(forms))
  {
    bad_forms <- forms[!forms %in% MetaData$form_name]
    if (length(bad_forms))
      coll$push(paste0("The following are not valid form names: ",
                       paste0(bad_forms, collapse = ", ")))
  }
  
  checkmate::reportAssertions(coll)
  
   ##################################################################
  # Create the vector of field names
  if (!is.null(fields)) #* fields were provided
  {
    # redcap_event_name is automatically included in longitudinal projects
    field_names <- fields[!fields %in% "redcap_event_name"] 
  }
  else if (!is.null(forms))
  {
    field_names <- MetaData$field_name[MetaData$form_name %in% forms]
  }
  else
    #* fields were not provided, default to all fields.
    field_names <- MetaData$field_name
  
   ##################################################################
  # Expand 'field_names' to include fields from specified forms.    
  if (!is.null(forms)) 
  {
    field_names <- 
      unique(c(field_names, 
               MetaData$field_name[MetaData$form_name %in% forms]))
  }  
  
   ##################################################################
  # Manage checkbox suffixes
  
  suffixed <- checkbox_suffixes(fields = field_names,
                                meta_data = MetaData)
  
  
   ##################################################################
  # Load and process data
  
  Records <- utils::read.csv(dataFile, 
                             stringsAsFactors = FALSE,
                             colClasses = colClasses)
  
  Records <- fieldToVar(records = Records, 
                        meta_data = MetaData, 
                        factors = factors, 
                        dates = dates, 
                        labels=labels,
                        checkboxLabels = checkboxLabels,
                        ...)
  
  if (labels){
    Records[,suffixed$name_suffix] <-
      mapply(nm = suffixed$name_suffix,
             lab = suffixed$label_suffix,
             FUN = function(nm, lab){
               if(is.null(Records[[nm]])){
                 warning("Missing field for suffix ", nm)
               } else {
                 labelVector::set_label(Records[[nm]], lab)
               }
             },
             SIMPLIFY = FALSE)
  }
  
  
  # drop
  if(length(drop)) {
    Records <- Records[!names(Records) %in% drop]
  } # end drop
  
  Records
}

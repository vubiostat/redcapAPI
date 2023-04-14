
#' @name mChoiceCast
#' 
#' @title A helper function that adds the \code{Hmisc::mChoice} multiple 
#'        choice class.
#' @description Adds a column for a multiple choice checkbox that is cast
#' to the \code{Hmisc::mChoice} class. Requires \code{Hmisc} to be 
#' loaded.
#'
#' @param Records A \code{data.frame} containing the records from
#'        \code{\link{exportRecordsTyped}}
#' @param rcon A REDCap connection object as created by \code{redcapConnection}.
#' @export
#' @examples
#' \dontrun{
#' recs <- exportRecordsTyped(rcon) |> mChoiceCast(rcon)
#' }
mChoiceCast <- function(Records, rcon, style="labelled")
{
   ###################################################################
  # Check arguments
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x       = Records,
                          classes = "data.frame",
                          add     = coll)
  
  checkmate::assert_class(x       = rcon,
                          classes = "redcapApiConnection",
                          add     = coll)
  style <- checkmate::matchArg(x = style, 
                               choices = c("coded", "labelled"), 
                               add = coll)
  ## FIXME: How to assert? if("package:Hmisc" %in% search())
    
  checkmate::reportAssertions(coll)
  
  fields <- colnames(Records)

# # .exportRecordsTyped_addmChoiceField -------------------------------
# .exportRecordsTyped_addmChoiceField <- function(Records, 
#                                                 Raw, 
#                                                 rcon, 
#                                                 fields, 
#                                                 mChoice){
  
  # FIXME: Where does fields come from?
  # FIXME: Where does Raw come from? or could we substitute the recast function?
  #   Even better why bother? Let the other field be cast in the desired manner.
  
  MetaData <- rcon$metadata()
  CheckboxMetaData <- MetaData[MetaData$field_type == "checkbox", ]
  checkbox_fields <- fields[fields %in% CheckboxMetaData$field_name]
  
  for (i in seq_along(checkbox_fields))
    Records[[ checkbox_fields[i] ]] <- 
      .mChoiceField(rcon, 
                    records_raw = Raw, 
                    checkbox_fieldname = checkbox_fields[i], 
                    style = style)

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
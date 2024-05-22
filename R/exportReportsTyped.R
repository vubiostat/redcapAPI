#' @describeIn recordsTypedMethods Export reports with type casting.
#' @order 2
#' @export
#' @export

exportReportsTyped <- function(rcon, 
                               report_id, 
                               ...){
  UseMethod("exportReportsTyped")
}

#' @rdname recordsTypedMethods
#' @order 5
#' @export

exportReportsTyped.redcapApiConnection <- function(rcon, 
                                                   report_id, 
                                                   drop_fields   = NULL, 
                                                   
                                                   # Type Casting Default Overrides Function Lists
                                                   na            = list(),
                                                   validation    = list(),
                                                   cast          = list(),
                                                   assignment    = list(label=stripHTMLandUnicode,
                                                                        units=unitsFieldAnnotation),
                                                   warn_zero_coded = TRUE,
                                                   ...,
                                                   csv_delimiter = ",")
{
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_integerish(x = report_id, 
                               len = 1, 
                               any.missing = FALSE, 
                               add = coll)
  
  checkmate::assert_character(x = drop_fields, 
                              any.missing = FALSE, 
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_list(x = na, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = validation, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = cast, 
                         names = "named", 
                         add = coll)

  checkmate::assert_list(x = assignment, 
                         names = "named", 
                         add = coll)

  csv_delimiter <- checkmate::matchArg(x = csv_delimiter, 
                                       choices = c(",", "\t", ";", "|", "^"),
                                       .var.name = "csv_delimiter",
                                       add = coll)
  
  checkmate::reportAssertions(coll)
  
  MetaData <- rcon$metadata()
  ProjectFields <- rcon$fieldnames()
  available_fields <- unique(c(ProjectFields$original_field_name, 
                               ProjectFields$export_field_name, 
                               MetaData$field_name[MetaData$field_type %in% c("calc", "file")], 
                               REDCAP_SYSTEM_FIELDS))
  
  checkmate::assert_subset(x = drop_fields, 
                           choices = available_fields, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make the Body List                                           ####
  
  body <- list(content = "report", 
               report_id = report_id, 
               format = "csv", 
               returnFormat = "csv", 
               csvDelimiter = csv_delimiter)

  ###################################################################
  # Call the API                                                 ####
  Raw <- as.data.frame(
           makeApiCall(rcon, body, ...),
           sep = csv_delimiter)

  if (length(drop_fields) > 0)
    Raw <- Raw[!names(Raw) %in% drop_fields]

  ###################################################################
  # Cast the fields in the report                                ####
  
  # See fieldCastingFunctions.R for definition of .castRecords
  .castRecords(Raw              = Raw, 
               Records          = NULL,
               rcon             = rcon, 
               na               = na, 
               validation       = validation, 
               cast             = cast, 
               assignment       = assignment, 
               default_cast     = .default_cast, 
               default_validate = .default_validate,
               warn_zero_coded  = warn_zero_coded)
}

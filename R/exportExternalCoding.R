#' @name exportExternalCoding
#' @title Export Codebook Mappings for Fields with External Dependencies
#' 
#' @description These methods enable `redcapAPI` to obtain a mapping of 
#'   codes and associated labels for fields that have external dependencies. 
#'   The fields include SQL fields (dependent on another project) or 
#'   fields that utilize the BioPortal Ontology modules.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @inheritParams recordsTypedMethods
#' 
#' @details These methods operate by executing two API calls to export first the 
#'   coded values and then the labeled values of fields with external 
#'   dependencies. The two exports are then used to generate the code-label 
#'   mappings for use in casting data.
#'   
#'   Fields of type `sql` are dropdown fields that are populated by a SQL 
#'   query to another project. 
#'   
#'   Fields of type `bioportal` are text fields that have the BioPortal 
#'   Ontology module enabled as the validation method.
#'   
#' @return
#' Returns a named list of named character vectors. 
#' 
#' Each element is in the list is named for the field it maps. 
#' 
#' The character vectors are name-value pairs where the name is the labeled 
#' data and the value is the coded data.
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' exportExternalCoding(rcon)
#' }
#'   

exportExternalCoding <- function(rcon, 
                                 fields, 
                                 ...){
  UseMethod("exportExternalCoding")
}

#' @rdname exportExternalCoding
#' @export

exportExternalCoding.redcapApiConnection <- function(rcon, 
                                                     fields         = NULL, 
                                                     ..., 
                                                     batch_size     = 1000)
{
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          class = "redcapConnection", 
                          add = coll)
  
  checkmate::assert_character(x = fields, 
                              null.ok = TRUE, 
                              any.missing = FALSE, 
                              add = coll)
  
  checkmate::assert_integerish(x = batch_size, 
                               len = 1,
                               lower = 1, 
                               null.ok = TRUE, 
                               any.missing = FALSE, 
                               add = coll)

  checkmate::reportAssertions(coll)
  
  checkmate::assert_subset(x = fields, 
                           choices = rcon$metadata()$field_name, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Functionality                                                ####

  MetaData <- rcon$metadata()
  external_fields <- 
    MetaData$field_name[grepl("BIOPORTAL", 
                              MetaData$select_choices_or_calculations, 
                              ignore.case = TRUE) | 
                       (!is.na(MetaData$field_type) & MetaData$field_type == "sql")]
  
  if (is.null(fields)){
    fields <- external_fields
  } else {
    fields <- fields[fields %in% external_fields]
  }
  
  if (length(fields) == 0){
    return(list())
  }
  
  body <- c(list(content = "record", 
                 format = "csv", 
                 returnFormat = "csv", 
                 type = "flat", 
                 rawOrLabel = "raw"), 
            vectorToApiBodyList(fields, "fields"))

  Code <- 
    if (!is.null(batch_size)){
      .exportRecordsTyped_Batched(rcon = rcon, 
                                  body = body, 
                                  records = NULL,
                                  csv_delimiter = ",", 
                                  batch_size = batch_size,
                                  ...)
    } else {
      .exportRecordsTyped_Unbatched(rcon = rcon, 
                                    body = body, 
                                    records = NULL,
                                    csv_delimiter = ",",
                                    ...)
    }
  
  body$rawOrLabel <- "label"
  
  Label <- 
    if (!is.null(batch_size)){
      .exportRecordsTyped_Batched(rcon = rcon, 
                                  body = body, 
                                  records = NULL,
                                  csv_delimiter = ",", 
                                  batch_size = batch_size,
                                  ...)
    } else {
      .exportRecordsTyped_Unbatched(rcon = rcon, 
                                    body = body, 
                                    records = NULL, 
                                    csv_delimiter = ",",
                                    ...)
    }
  
  External <- vector("list", length(fields))
  names(External) <- fields
  
  for (f in fields){
    ThisCode <- data.frame(code = Code[[f]], 
                           label = Label[[f]], 
                           stringsAsFactors = FALSE)
    ThisCode <- ThisCode[!duplicated(ThisCode), ]
    ThisCode <- ThisCode[!is.na(ThisCode$code), ]

    mapping <- ThisCode$code
    names(mapping) <- ThisCode$label
    
    External[[f]] <- mapping
  }
  
  External
}

#' @name exportBioportalCoding
#' @title Export Codebook Mapping for BioPortal Fields
#' 
#' @description These methods enable `redcapAPI` to obtain a mapping of 
#'   codes and associated labels for BioPortal fields. 
#'   BioPortal fields are text fields that have enabled the 
#'   BioPortal Ontology Service Validation. The mapping between coded and 
#'   labeled values is not exported with the meta data for these fields. 
#'   Due to their size, it is impractical to attempt to maintain these tables
#'   within `redcapAPI`.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @inheritParams recordsTypedMethods
#' 
#' @details The methods operate by executing two API calls to export first the 
#'   coded values and then the labeled values of BioPortal fields. The 
#'   two exports are then used to generate the code-label mappings for use 
#'   in casting data. 
#'   
#' @return
#' Returns a named list. Each element is in the list is named for the field
#'   it maps. The elements are named vectors where the name is the label
#'   of the BioPortal field and the value is the code.
#'   
#' @export

exportBioportalCoding <- function(rcon, 
                                  fields, 
                                  ...){
  UseMethod("exportBioportalCoding")
}

#' @rdname exportBioportalCoding
#' @export

exportBioportalCoding.redcapApiConnection <- function(rcon, 
                                                      fields = NULL, 
                                                      ..., 
                                                      batch_size = 1000, 
                                                      error_handling = getOption("redcap_error_handling"), 
                                                      config = list(), 
                                                      api_param = list()){
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
  
  error_handling <- checkmate::matchArg(x = error_handling,
                                        choices = c("null", "error"),
                                        .var.name = "error_handling",
                                        add = coll)
  
  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         names = "named", 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  checkmate::assert_subset(x = fields, 
                           choices = rcon$metadata()$field_name, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Functionality                                                ####

  MetaData <- rcon$metadata()
  bioportal_fields <- 
    MetaData$field_name[grepl("BIOPORTAL", 
                              MetaData$select_choices_or_calculations, 
                              ignore.case = TRUE)]
  
  if (is.null(fields)){
    fields <- bioportal_fields
  } else {
    fields <- fields[fields %in% bioportal_fields]
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
  
  body <- body[lengths(body) > 0]
  
  Code <- .exportRecordsTyped_Batched(rcon = rcon, 
                                      body = body, 
                                      records = NULL, 
                                      config = config, 
                                      api_param = api_param, 
                                      csv_delimiter = ",", 
                                      batch_size = batch_size, 
                                      error_handling = error_handling)
  
  body$rawOrLabel <- "label"
  
  Label <- .exportRecordsTyped_Batched(rcon = rcon, 
                                       body = body, 
                                       records = NULL, 
                                       config = config, 
                                       api_param = api_param, 
                                       csv_delimiter = ",", 
                                       batch_size = batch_size, 
                                       error_handling = error_handling)
  
  BioPortal <- vector("list", length(fields))
  names(BioPortal) <- fields
  
  for (f in fields){
    ThisCode <- data.frame(code = Code[[f]], 
                           label = Label[[f]], 
                           stringsAsFactors = FALSE)
    ThisCode <- ThisCode[!duplicated(ThisCode), ]
    ThisCode <- ThisCode[!is.na(ThisCode$code), ]

    mapping <- ThisCode$code
    names(mapping) <- ThisCode$label
    
    BioPortal[[f]] <- mapping
  }
  
  BioPortal
}

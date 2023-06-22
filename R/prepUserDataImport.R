#' @name prepUserImportData
#' @title Prepare User Data for Import
#' 
#' @description Prepares a data frame for import via the API. Allows for 
#'   data to be passed in either the raw format or the labelled data 
#'   received from \code{exportUsers}.
#'   
#' @param data \code{data.frame} that has the structure of 
#'   \code{redcapAPI:::REDCAP_USER_STRUCTURE}. It may also have additional 
#'   columns for the form and export access of each of the instruments.
#' @param rcon \code{redcapConnection}. Used to determine the instruments
#'   in the project.
#' @param priority \code{character}, one of \code{c("expanded", "consolidated")}. 
#'   If \code{"expanded"}, the form and data export access values will be 
#'   read from the expanded columns. Otherwise, the consolidated values 
#'   (as provided by the API export) are utilized.
#'   
#' @export

prepUserImportData <- function(data, 
                               rcon, 
                               priority = c("expanded", "consolidated")){
  
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_data_frame(x = data, 
                               names = "named", 
                               add = coll)
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapConnection", 
                          add = coll)
  
  priority <- checkmate::matchArg(x = priority, 
                                  choices = c("expanded", "consolidated"), 
                                  add = coll)
  
  checkmate::reportAssertions(coll)
  
  primary_fields <- names(data)
  primary_fields <- primary_fields[!grepl("(_export_access|_form_access)$", 
                                          primary_fields)]
  checkmate::assert_subset(x = primary_fields, 
                           choices = names(REDCAP_USER_STRUCTURE), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Functional Code                                              ####
  
  fields_to_remove <- c("email", "lastname", "firstname", 
                        "data_access_group_id")
  data <- data[!names(data) %in% fields_to_remove]
  
}

#####################################################################
# Unexported                                                     ####



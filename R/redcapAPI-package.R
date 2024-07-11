#' Access data, meta data, and files from REDCap using the API
#'
#' REDCap is a database development tool built on MySQL.  Visit 
#' \url{https://projectredcap.org} for more information.  REDCap
#' provides an API through which data, the data dictionary, files, and
#' project information can be accessed.  The `redcapAPI` package 
#' facilitates the use of these functions and 
#' simplifies the work needed to prepare data for 
#' analysis.
#' 
#' The user should also refer to their institution's REDCap API 
#' documentation as a primary resource of what is available.  Different 
#' versions of REDCap support different features--the user's REDCap 
#' API documentation will address the features specific to their version 
#' of REDCap.
#' 
#' @name redcapAPI
#' @keywords internal
#' @import checkmate 
#' @importFrom chron times
#' @importFrom getPass getPass
#' @importFrom httr config content POST set_config upload_file
#' @importFrom keyring key_delete key_get key_list key_set_with_value keyring_create keyring_list keyring_unlock
#' @importFrom labelVector get_label is.labelled set_label
#' @importFrom lubridate parse_date_time
#' @importFrom stats reshape
#' @importFrom utils capture.output compareVersion head modifyList
#' osVersion packageVersion read.csv tail write.csv write.table
#' @importFrom yaml read_yaml

"_PACKAGE"

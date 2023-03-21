#' @name checkbox_suffixes 
#' @title Checkbox Suffixes
#' 
#' @description Checkbox variables return one vector of data for each option defined
#'   in the variable.  The variables are returned with the suffix \code{___[option]}.
#'   \code{exportRecords} needs these suffixes in order to retrieve all of the 
#'   variables and to apply the correct labels.
#'   
#' @param fields The current field names of interest
#' @param meta_data The meta data data frame.
#' 
#' @export

checkbox_suffixes <- function(fields, meta_data)
{
  name_suffix <- sapply(X = fields, 
                        FUN = manual_checkbox_suffixes, 
                        meta_data, 
                        simplify = FALSE)

  label_suffix <- 
    sapply(X = fields,
           FUN = manual_checkbox_label_suffixes,
           meta_data, 
           simplify = FALSE)
  
  list(name_suffix = unlist(name_suffix),
       label_suffix = unlist(label_suffix))
}

#***********************************************
#* Unexported methods

#* Get full variable names (appends ___[option] to checkboxes)
manual_checkbox_suffixes <- function(x, meta_data)
{
  #* If x is a checkbox variable
  if (meta_data$field_type[meta_data$field_name %in% x] == "checkbox"){
    field_choice <- meta_data$select_choices_or_calculations[meta_data$field_name %in% x]
    opts <- fieldChoiceMapping(field_choice)
    opts <- tolower(opts)[, 1]
    
    x <- paste(x, opts, sep="___")
  }
  x
}

#* Get full variable label (appends ": [option label]" for checkboxes)
manual_checkbox_label_suffixes <- function(x, meta_data)
{
  #* If x is a checkbox variable
  if (meta_data$field_type[meta_data$field_name %in% x] == "checkbox"){
    #* Select choices
    field_choice <- meta_data$select_choices_or_calculations[meta_data$field_name %in% x]
    opts <- fieldChoiceMapping(field_choice)[, 2]

    paste0(meta_data$field_label[meta_data$field_name %in% x], ": ", opts)
  }
  else 
  {
    meta_data$field_label[meta_data$field_name %in% x]
  }
}

#' @name fieldToVar
#' 
#' @title Convert a REDCap Data Field to an R Vector
#' @description Converts a field exported from REDCap into a valid R vector
#' 
#' @param records A data frame of records returned by `exportRecords` 
#'   or `exportReports`
#' @param meta_data A data frame giving the data dictionary, as returned 
#'   by `exportMetaData`
#' @param factors Logical, determines if checkbox, radio button, dropdown and yesno
#'   variables are converted to factors
#' @param dates Logical, determines if date variables are converted to POSIXct format
#' @param checkboxLabels Logical, determines if checkbox variables are labeled as
#'   "Checked" or using the checkbox label.  Only applicable when `factors = TRUE`
#' @param labels Logical.  Determines if the variable labels are applied to 
#'   the data frame.
#' @param handlers List, Specify type conversion overrides for specific REDCap field types. 
#'   E.g., `handlers=list(date_ = as.Date)`. For datetime specifications the
#'   datetime ordering directive from the tail is dropped. The following field
#'   types are supported: date_, datetime_, datetime_seconds_, time_mm_ss, time,
#'   float, number, calc, int, integer, select, radio, dropdown, yesno, truefalse,
#'   checkbox, and form_complete.
#' @param mChoice logical; defaults to TRUE. Convert checkboxes to mChoice if
#'   Hmisc is installed.
#' @param ..., additional arguments that are ignored. 
#'   
#' @details This function is called internally by `exportRecords` and 
#'   `exportReports`.  it is not available to the user.
#'   
fieldToVar <- function(records,
                       meta_data,
                       factors        = TRUE, 
                       dates          = TRUE,
                       checkboxLabels = FALSE,
                       labels         = TRUE,
                       handlers       =list(),
                       mChoice        = NULL,
                       ...)
{ 
  records_raw <- records
  
  # See if mChoice argument is passed, otherwise default to state of Hmisc
  if("package:Hmisc" %in% search()) # Hmisc Loaded?
  {
    if(is.null(mChoice)) mChoice <- TRUE
    # Otherwise do what user requests for mChoice
  } else # Hmisc not loaded
  {
    if(is.null(mChoice)) 
    {
      mChoice <- FALSE
    } else if(mChoice)
    {
      warning("mChoice=TRUE requires the package Hmisc to be loaded to function properly.")
      mChoice <- FALSE
    }
  }
  
  recordnames <- names(records)
  for (i in seq_along(records))
  {
    # Establish basic info about field/record
    field_name <- recordnames[i]
    field_base <- sub(REGEX_CHECKBOX_FIELD_NAME, #defined in constants.R 
                      "\\1", field_name, perl = TRUE)
    field_text_type <- meta_data$text_validation_type_or_show_slider_number[meta_data$field_name == field_base]
    field_type <- meta_data$field_type[meta_data$field_name == field_base]
    
    withCallingHandlers(
      warning=function(w) {w$message<-paste(field_name,":", w$message);
                           warning(w);
                           invokeRestart("muffleWarning")},
    {
      #* If the variable is not in the data dictionary (usually it's a field added by REDCap,
      #* such as redcap_event_name, instrument_complete, etc), give it a generic name to
      #* pass to switch.
      if (!length(field_type)) 
      {
        if (grepl("_complete$", field_base))
        {
          field_type <- "form_complete"
        }
        else  
        {
          field_type <- "unrecognized field type"
        }
      }
      # autocomplete was added to the text_validation... column for
      # dropdown menus with the autocomplete feature.
      # field_type[is.na(field_type)] <- 
      #   meta_data$field_type[meta_data$field_name == field_base]
      field_type[field_type == "text" & 
                   !is.na(field_text_type)] <- field_text_type
  
      field_type <- gsub(pattern = "_(dmy|mdy|ymd)$", 
                         replacement = "_",
                         x = field_type)
      
      records[[i]] <-
        if(field_type %in% names(handlers))
        {
          handlers[[field_type]](records[[i]]) 
        } else {
          switch(field_type,
                 "date_" = 
                   {
                     if (dates) 
                       as.POSIXct(records[[i]], format = "%Y-%m-%d") 
                     else 
                       records[[i]]
                    },
                 "datetime_" = 
                   {
                     if (dates) 
                       as.POSIXct(records[[i]], format = "%Y-%m-%d %H:%M") 
                     else 
                       records[[i]]
                   },
                 "datetime_seconds_" = 
                   {
                     if (dates) 
                       as.POSIXct(records[[i]], format = "%Y-%m-%d %H:%M:%S") 
                     else 
                       records[[i]]
                   },
                 "time_mm_ss" = 
                   {
                     if (dates) 
                       chron::times(ifelse(!is.na(records[[i]]), 
                                           paste0("00:", records[[i]]), 
                                           records[[i]]), 
                                    format=c(times="h:m:s"))
                     else 
                       records[[i]]
                   },
                 "time_hh_mm_ss" = 
                   {
                     if (dates) 
                       chron::times(records[[i]], 
                                    format=c(times="h:m:s"))
                     else 
                       records[[i]]
                   },
                 "time" = 
                   {
                     if (dates)
                       chron::times(gsub("(^\\d{2}:\\d{2}$)", "\\1:00", records[[i]]), 
                                    format=c(times="h:m:s"))
                     else 
                       records[[i]]
                   },
                 "float"  = as.numeric(records[[i]]),
                 "number" = as.numeric(records[[i]]),
                 "calc"   = as.numeric(records[[i]]),
                 "int"    = as.integer(records[[i]]),
                 "integer"= as.numeric(records[[i]]),
                 "select" = 
                   makeRedcapFactor(x = records[[i]],
                                    coding = meta_data$select_choices_or_calculations[meta_data$field_name == field_base],
                                    factors = factors, 
                                    var_name = meta_data$field_name[meta_data$field_name == field_base]),
                 "radio" = 
                   makeRedcapFactor(x = records[[i]],
                                    coding = meta_data$select_choices_or_calculations[meta_data$field_name == field_base],
                                    factors = factors, 
                                    var_name = meta_data$field_name[meta_data$field_name == field_base]),
                 "dropdown" = 
                   makeRedcapFactor(x = records[[i]],
                                    coding = meta_data$select_choices_or_calculations[meta_data$field_name == field_base],
                                    factors = factors, 
                                    var_name = meta_data$field_name),
                 "yesno" = makeRedcapYN(records[[i]], 
                                        factors),
                 "truefalse" = 
                  {
                    if (factors) 
                      as.logical(records[[i]])
                    else
                      records[[i]]
                  },
                 "checkbox" = 
                  {
                    makeRedcapCheckbox(x = records[[i]],
                                       suffix = gsub("^.+___", "", names(records)[i]),
                                       coding = meta_data$select_choices_or_calculations[meta_data$field_name == field_base],
                                       factors = factors,
                                       checkboxLabels = checkboxLabels)
                  },
                 "form_complete" = 
                 {
                   makeRedcapFactor(x = records[[i]],
                                    coding = "0, Incomplete | 1, Unverified | 2, Complete",
                                    factors, 
                                    var_name = meta_data$field_name[meta_data$field_name == field_base])
                 },
                 records[[i]]
          ) # End switch
        } # End of Records[[i]] if
    }) # End of withCallingHandlers
  } # End for loop
  
  if(mChoice)
  {
    # Convert checkboxes to mChoice if Hmisc is installed and requested
    checkbox_meta <- meta_data[which(meta_data$field_type == 'checkbox'),]
    for(i in seq_len(nrow(checkbox_meta)))
    {
      checkbox_fieldname <- checkbox_meta$field_name[i]
      fields <- recordnames[grepl(sprintf("^%s", checkbox_fieldname), recordnames)]
      if(length(fields) > 0)
      {
        opts <- fieldChoiceMapping(checkbox_meta[i,'select_choices_or_calculations'], 
                                   fields[i])
        levels <- opts[, 1 + labels]

        opts <- as.data.frame(matrix(rep(seq_along(fields), nrow(records)), nrow=nrow(records), byrow=TRUE))
        checked <- records_raw[,fields] != '1'
        opts[which(checked,arr.ind=TRUE)] <- ""
        z <- structure(
          gsub(";$|^;", "",gsub(";{2,}",";", do.call('paste', c(opts, sep=";")))),
          label  = checkbox_fieldname,
          levels = levels,
          class  = c("mChoice", "labelled"))
  
        records[[checkbox_fieldname]] <- z
      }
    }

  } # mChoice 
  
  records
}    



  

    

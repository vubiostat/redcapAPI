#' @name widerRepeated
#' @title Transform Data Into Wide Format
#' 
#' @description Converts a dataframe into wide format given the column names to expand.
#'   
#' @param Records A \code{data.frame} containing the records from
#'        \code{\link{exportRecordsTyped}}
#' @param rcon A REDCap connection object as created by \code{unlockREDCap}.
#' @param idvar \code{character}, Names of one or more variables in long format that identify multiple records belonging to the same group. 
#'        These variables may also be present in wide format. (e.g., 'record_id')


widerRepeated <- function(rcon, idvar){
  # export data dictionary
  meta <- rcon$metadata()
  
  # export repeating
  repeating <- rcon$repeatInstrumentEvent()
  
  # export events
  events <- rcon$events()
  
  # export mappings
  mappings <- rcon$mapping()
  
  # export data
  records <- exportRecordsTyped(rcon)
  
  # helper function
  is.blank <- function(x) is.na(x) | x == ""
  
  # set everything as character in data and data dictionary and assign id.temp to data
  dat <- data.frame(lapply(records, as.character), stringsAsFactors = FALSE)
  names(dat)[names(dat) == idvar] <- "id.tmp"
  
  dd <- data.frame(lapply(meta, as.character), stringsAsFactors = FALSE)
  
  # get correct event mappings if redcap_event_name is present or not
  if ("redcap_event_name" %in% names(dat)) {
    #get the event-form mapping
    event.map <- mappings
  } else {
    event.map = data.frame(
      unique_event_name = "",
      form = rcon$instruments()$instrument_name)
  }
  
  # because not all projects will have all id modifier fields
  df.all_ids <- data.frame(id.tmp = character(0), redcap_event_name = character(0), redcap_repeat_instrument = character(0), redcap_repeat_instance = character(0))
  tmpd <- merge(
    data.frame(dat),
    df.all_ids,
    by = names(df.all_ids)[
      names(df.all_ids) %in% names(dat)],
    all = TRUE)
  tmpd[is.na(tmpd)] <- ""
  
  id.fields = names(df.all_ids)
  
  map <- event.map
  data.frame(map)
  map[is.na(map)] <- ""
  
  for (i in sort(unique(dd$form_name))) {
    vars.tmp <- names(dat)[sub(REGEX_CHECKBOX_FIELD_NAME, "\\1", names(dat)) %in% dd$field_name[dd$form_name == i]]
    
    if (length(vars.tmp) == 0) next #form with descriptive only
    
    tmp <- subset(tmpd, tmpd$redcap_event_name %in% map[map$form == i, "unique_event_name"] & (tmpd$redcap_repeat_instrument == i | is.blank(tmpd$redcap_repeat_instrument)),
      select = c(id.fields, vars.tmp))
    
    # reshape to eliminate empty instances or any variable empty in all instances
    tmp_long <- reshape(tmp[, c(id.fields, vars.tmp)], 
                        varying = list(vars.tmp),
                        times = vars.tmp,
                        direction = "long")
    tmp <- tmp_long[complete.cases(tmp_long), ]
    
    if (nrow(tmp) > 0) {
      # Convert tmp to wide format
      tmp <- reshape(tmp, idvar = c(id.fields), direction = "wide")
      # drop redcap_repeat_instrument column
      tmp$redcap_repeat_instrument <- NULL
      # add form column
      tmp$name.form <- i
      
    } else {
      tmp <- data.frame(
        matrix(
          ncol = length(c(id.fields, vars.tmp)),
          nrow = 0))
      names(tmp) = c(id.fields, vars.tmp)
      data.frame(tmp)
    }
    
    what.has <- with(
      tmp,
      c(
        all(is.blank(redcap_event_name)),
        all(is.blank(redcap_repeat_instance))))
    if (what.has[[1]] == TRUE) tmp <- tmp$redcap_event_name <- NULL
    if (what.has[[2]] == TRUE) tmp <- tmp$redcap_event_name <- NULL
    
    tmp2 <- list(tmp)
    names(tmp2) <- i
    
    if (exists("list.forms")) {
      list.forms = c(tmp2, list.forms)
    } else {
      list.forms = tmp2
    }
    rm(i, tmp)
  }
  return(list.forms)
}


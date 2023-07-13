context("importRecords Functionality")

rec <- exportRecords(rcon, mChoice=FALSE)
rows <- nrow(rec)

NewRecords <- rbind(rec[1,], rec[1,])
NewRecords$record_id <- c("delete.me", "delete.too")

#####################################################################
# Tests

test_that(
  "importRecords overwriteBehavior", 
  {
    # Import new records for the test
    importRecords(rcon, 
                  data = NewRecords)
    
    # Change a value in the new record
    NewNewRecords <- NewRecords
    NewNewRecords$date_dmy <- ""
    
    # overwriteBehavior = normal produces no change
    
    importRecords(rcon, 
                  data = NewNewRecords, 
                  overwriteBehavior = "normal")
    
    suppressWarnings({
      Compare <- exportRecords(rcon, 
                               records = NewNewRecords$record_id, 
                               fields = "date_dmy")
    })
      
    expect_true(all(!is.na(Compare$date_dmy)))
    
    # overwriteBehavior = overwrite produces a change
    
    importRecords(rcon, 
                  data = NewNewRecords, 
                  overwriteBehavior = "overwrite")
    
    suppressWarnings({
      Compare <- exportRecords(rcon, 
                               records = NewNewRecords$record_id, 
                               fields = "date_dmy")
    })
    
    expect_true(all(is.na(Compare$date_dmy)))
    
    # Cleanup 
    deleteRecords(rcon, 
                  records = NewRecords$record_id)
  }
)

test_that(
  "importRecords returnContent", 
  {
    # Import new records returnContent = count returns count of records imported
    expect_equal(importRecords(rcon, 
                               data = NewRecords, 
                               returnContent = "count"), 
                 "2")
    
    # Import new records returnContent = ids returns IDs of records imported
    
    expect_equal(importRecords(rcon, 
                               data = NewRecords, 
                               returnContent = "ids"), 
                 data.frame(id = c("delete.me", "delete.too")))
    
    # Import new records returnContent = nothing returns ""
    expect_equal(importRecords(rcon, 
                               data = NewRecords, 
                               returnContent = "nothing"), 
                 "")
    
    # clean up
    deleteRecords(rcon, 
                  records = c("delete.me", "delete.too"))
  }
)

test_that(
  "importRecords returnData", 
  {
    # Return the formatted data frame ready for import
    
    Prepped <- importRecords(rcon, 
                             data = NewRecords, 
                             returnData = TRUE)
    
    expect_data_frame(Prepped)
    
    # Checked and unchecked are not 0/1
    expect_true(all(Prepped$prereq_checkbox___1 %in% c(0, 1)))
  }
)

test_that(
  "Save log to a file", 
  {
    # Save the import validation to a file
    logfile_path <- file.path(tempdir(), "logfile.txt")
    
    importRecords(rcon, 
                  data = NewRecords, 
                  logfile = logfile_path, 
                  returnData = TRUE)
    
    expect_true(file.exists(logfile_path))
    file.remove(logfile_path)
  }
)

test_that(
  "mChoice fields are dropped", 
  {
    local_reproducible_output(width = 200)
    require(Hmisc)
    TheData <- exportRecordsTyped(rcon)
    WithMChoice <- mChoiceCast(TheData, rcon)
    WithMChoice <- suppressWarnings(castForImport(WithMChoice, rcon))
    expect_message(importRecords(rcon, WithMChoice), 
                   "The variable[(]s[)] file_import_field, signature_test, file_upload_test, prereq_checkbox, no_prereq_checkbox, checkbox_test are not found in the project and/or cannot be imported.")
    TheDataAfter <- exportRecordsTyped(rcon)
    expect_true(identical(TheData, TheDataAfter))
    detach("package:Hmisc", unload = TRUE)
  }
)

test_that(
  "import with Auto Numbering", 
  {
    Records <- exportRecordsTyped(rcon, mChoice = FALSE, 
                                  cast = list(system = castRaw))
    nrow(Records)
    OneRecord <- Records[1,]
    
    next_record_id <- exportNextRecordName(rcon)
    
    NewId <- importRecords(rcon, 
                           data = OneRecord, 
                           returnContent = 'auto_ids',
                           force_auto_number = TRUE)
    
    expect_true(NewId$id > nrow(Records))
    
    After <- exportRecordsTyped(rcon)
    
    expect_true(nrow(Records) < nrow(After))
    
    deleteRecords(rcon, records = max(After$record_id))
  }
)

#####################################################################
# Tests for dates with validation                                ####
# See Issue 176

test_that(
  "Validations of `today` and `now`",
  {
    # Date variable with today in min validation ####
    OrigMeta <- rcon$metadata()
    w_var <- which(OrigMeta$field_name == "date_ymd_test")

    OrigMeta$text_validation_min[w_var] <- "today"
    importMetaData(rcon, OrigMeta)
    
    NewData <- exportRecordsTyped(rcon, cast = list(system = castRaw))
    NewData <- NewData[NewData$record_id == 10, 
                       c("record_id", "redcap_event_name", 
                         "redcap_repeat_instrument", "redcap_repeat_instance", 
                         "redcap_data_access_group", "date_ymd_test")]
    expect_equal(importRecords(rcon, NewData), "1")
    
    OrigMeta$text_validation_min[w_var] <- NA_character_
    importMetaData(rcon, OrigMeta)
    
    # Date variable with today in max validation ####
    OrigMeta <- rcon$metadata()
    w_var <- which(OrigMeta$field_name == "date_ymd_test")
    
    OrigMeta$text_validation_max[w_var] <- "today"
    importMetaData(rcon, OrigMeta)
    
    NewData <- exportRecordsTyped(rcon, 
                                  cast = list(system = castRaw))
    NewData <- NewData[NewData$record_id == 10, 
                       c("record_id", "redcap_event_name", 
                         "redcap_repeat_instrument", "redcap_repeat_instance", 
                         "redcap_data_access_group", "date_ymd_test")]
    expect_equal(importRecords(rcon, NewData), "1")
    
    OrigMeta$text_validation_max[w_var] <- NA_character_
    importMetaData(rcon, OrigMeta)
    
    # Date/time variable with now in min validation ####
    OrigMeta <- rcon$metadata()
    w_var <- which(OrigMeta$field_name == "datetime_ymd_hms_test")
    
    OrigMeta$text_validation_min[w_var] <- "now"
    importMetaData(rcon, OrigMeta)
    
    NewData <- exportRecordsTyped(rcon, 
                                  cast = list(system = castRaw))
    NewData <- NewData[NewData$record_id == 10, 
                       c("record_id", "redcap_event_name", 
                         "redcap_repeat_instrument", "redcap_repeat_instance", 
                         "redcap_data_access_group", "date_ymd_test")]
    expect_equal(importRecords(rcon, NewData), "1")
    
    OrigMeta$text_validation_min[w_var] <- NA_character_
    importMetaData(rcon, OrigMeta)
    
    # Date/time variable with now in max validation ####
    OrigMeta <- rcon$metadata()
    w_var <- which(OrigMeta$field_name == "datetime_ymd_hms_test")
    
    OrigMeta$text_validation_max[w_var] <- "now"
    importMetaData(rcon, OrigMeta)
    
    NewData <- exportRecordsTyped(rcon, 
                                  cast = list(system = castRaw))
    NewData <- NewData[NewData$record_id == 10, 
                       c("record_id", "redcap_event_name", 
                         "redcap_repeat_instrument", "redcap_repeat_instance", 
                         "redcap_data_access_group", "date_ymd_test")]
    expect_equal(importRecords(rcon, NewData), "1")
    
    OrigMeta$text_validation_max[w_var] <- NA_character_
    importMetaData(rcon, OrigMeta)
    
  }
)

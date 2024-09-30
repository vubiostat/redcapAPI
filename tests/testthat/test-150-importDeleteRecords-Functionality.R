context("Import / Delete Records Methods Functionality")

load(file.path(test_path("testdata"),
               "test_redcapAPI_MetaData.Rdata"))
load(file.path(test_path("testdata"), 
               "test_redcapAPI_Data.Rdata"))
load(file.path(test_path("testdata"), 
               "test_redcapAPI_Arms.Rdata"))
load(file.path(test_path("testdata"), 
               "test_redcapAPI_Events.Rdata"))

fields <- c("record_id", "letters_only_test", "number_test", "date_dmy_test", 
            "left_operand", "calc_squared", 
            "prereq_checkbox___1", "prereq_checkbox___2", 
            "prereq_checkbox___3", "prereq_checkbox___4")
MetaData <- test_redcapAPI_MetaData[test_redcapAPI_MetaData$field_name %in% fields |
                                    test_redcapAPI_MetaData$field_name=='prereq_checkbox' , ]

importMetaData(rcon, MetaData)

ImportData <- test_redcapAPI_Data
ImportData <- ImportData[1, names(ImportData) %in% fields]


importArms(rcon,   test_redcapAPI_Arms)
importEvents(rcon, test_redcapAPI_Events)
importProjectInformation(rcon, data.frame(is_longitudinal = 1))

n <- length(rcon$instruments()$instrument_name)
importMappings(
  rcon, 
  data = data.frame(
    arm_num           = rep(1, n), 
    unique_event_name = rep("event_1_arm_1", n), 
    form              = rcon$instruments()$instrument_name
  )
)

#####################################################################
# Tests

test_that(
  "Import and Delete records", 
  {
    expect_data_frame(importRecords(rcon, 
                                    data = ImportData), 
                      nrows = 1)
    
    expect_equal(deleteRecords(rcon, 
                               records = 1), 
                 "1")
  }
)

test_that(
  "importRecords overwrite_behavior", 
  {
    # Import new records for the test
    importRecords(rcon, 
                  data = ImportData)
    
    # Change a value in the new record
    NewRecords <- ImportData
    NewRecords$date_dmy_test <- ""
    
    # overwriteBehavior = normal produces no change
    
    importRecords(rcon, 
                  data = NewRecords, 
                  overwriteBehavior = "normal")
    
    Compare <- exportRecordsTyped(rcon, 
                                  records = NewRecords$record_id, 
                                  fields = "date_dmy_test",
                                  cast = raw_cast)
    
    expect_true(all(!is.na(Compare$date_dmy)))
    
    # overwriteBehavior = overwrite produces a change
    
    importRecords(rcon, 
                  data = NewRecords, 
                  overwrite_behavior = "overwrite")
    
    Compare <- exportRecordsTyped(rcon, 
                                  records = NewRecords$record_id, 
                                  fields = "date_dmy_test", 
                                  cast = raw_cast)
    
    expect_true(all(is.na(Compare$date_dmy_test)))
    
    # Cleanup 
    deleteRecords(rcon, 
                  records = NewRecords$record_id)
  }
)

test_that(
  "importRecords return_content", 
  {
    # Import new records returnContent = count returns count of records imported
    expect_data_frame(importRecords(rcon, 
                                    data = ImportData, 
                                    returnContent = "count"), 
                      nrows = 1)
    
    # Import new records returnContent = ids returns IDs of records imported
    ThisImport <- expect_data_frame(importRecords(rcon, 
                                                  data = ImportData, 
                                                  return_content = "ids"), 
                                    nrows = 1)
    expect_equal(attr(ThisImport, "return_content"), 
                 data.frame(id = 1))
    
    # Import new records returnContent = nothing returns ""
    ThisImport <- importRecords(rcon, 
                                data = ImportData, 
                                returnContent = "nothing")
    expect_null(attr(ThisImport, "return_content"))
    
    # clean up
    deleteRecords(rcon, 
                  records = 1)
  }
)


test_that(
  "mChoice fields are handled", 
  {
    local_reproducible_output(width = 200)
    importRecords(rcon, 
                  data = ImportData[,])
                  
    require(Hmisc)
    TheData <- exportRecordsTyped(rcon)
    WithMChoice <- mChoiceCast(TheData, rcon)
    expect_error(importRecords(rcon, WithMChoice), 
                   ".*prereq_checkbox.*mChoice.*")
    expect_message(WithMChoice <- castForImport(WithMChoice, rcon), ".*mChoice.*dropped.*prereq_checkbox.*")
    expect_message(importRecords(rcon, WithMChoice), "1")
    detach("package:Hmisc", unload = TRUE)
  }
)

test_that(
  "import with Auto Numbering", 
  {
    importProjectInformation(rcon, 
                             data.frame(record_autonumbering_enabled = 1))
    Records <- exportRecordsTyped(rcon, mChoice = FALSE, 
                                  cast = list(system = castRaw))
    nrow(Records)
    OneRecord <- Records[1,]
    
    next_record_id <- exportNextRecordName(rcon)
    
    NewId <- importRecords(rcon, 
                           data = OneRecord, 
                           return_content = 'auto_ids',
                           force_auto_number = TRUE)
    
    expect_true(attributes(NewId)$return_content$id > nrow(Records))
    
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
    NewMetaData <- test_redcapAPI_MetaData
    NewMetaData <- NewMetaData[NewMetaData$field_name %in% c("record_id", "date_ymd_test"), ]
    
    importMetaData(rcon, 
                   NewMetaData)
    
    NewMetaData <- rcon$metadata()
    w_var <- which(NewMetaData$field_name == "date_ymd_test")
    
    NewMetaData$text_validation_min[w_var] <- "today"
    importMetaData(rcon, NewMetaData)
    
    NewData <- exportRecordsTyped(rcon, cast = list(system = castRaw))
    NewData <- NewData[NewData$record_id == 1, 
                       c("record_id", "redcap_event_name", "date_ymd_test")]
    expect_message(importRecords(rcon, NewData), "1")
    
    NewMetaData$text_validation_min[w_var] <- NA_character_
    importMetaData(rcon, NewMetaData)
    
    # Date variable with today in max validation ####
    NewMetaData <- rcon$metadata()
    w_var <- which(NewMetaData$field_name == "date_ymd_test")
    
    NewMetaData$text_validation_max[w_var] <- "today"
    importMetaData(rcon, NewMetaData)
    
    NewData <- exportRecordsTyped(rcon, 
                                  cast = list(system = castRaw))
    NewData <- NewData[NewData$record_id == 1, 
                       c("record_id", "redcap_event_name", 
                         "date_ymd_test")]
    expect_message(importRecords(rcon, NewData), "1")
    
    NewMetaData$text_validation_max[w_var] <- NA_character_
    importMetaData(rcon, NewMetaData)
    
    # Date/time variable with now in min validation ####
    
    NewMetaData <- test_redcapAPI_MetaData
    NewMetaData <- NewMetaData[NewMetaData$field_name %in% c("record_id", "datetime_ymd_hms_test"), ]
    
    importMetaData(rcon, 
                   NewMetaData)

    NewMetaData <- rcon$metadata()
    w_var <- which(NewMetaData$field_name == "datetime_ymd_hms_test")
    
    NewMetaData$text_validation_min[w_var] <- "now"
    importMetaData(rcon, NewMetaData)
    
    NewData <- exportRecordsTyped(rcon, 
                                  cast = list(system = castRaw))
    NewData <- NewData[NewData$record_id == 1, 
                       c("record_id", "redcap_event_name", "datetime_ymd_hms_test")]
    expect_message(importRecords(rcon, NewData), "1")
    
    NewMetaData$text_validation_min[w_var] <- NA_character_
    importMetaData(rcon, NewMetaData)
    
    # Date/time variable with now in max validation ####
    NewMetaData <- rcon$metadata()
    w_var <- which(NewMetaData$field_name == "datetime_ymd_hms_test")
    
    NewMetaData$text_validation_max[w_var] <- "now"
    importMetaData(rcon, NewMetaData)
    
    NewData <- exportRecordsTyped(rcon, 
                                  cast = list(system = castRaw))
    NewData <- NewData[NewData$record_id == 1, 
                       c("record_id", "redcap_event_name", "datetime_ymd_hms_test")]
    expect_message(importRecords(rcon, NewData), "1")
    
    NewMetaData$text_validation_max[w_var] <- NA_character_
    importMetaData(rcon, NewMetaData)
    
  }
)

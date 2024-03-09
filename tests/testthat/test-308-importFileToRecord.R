#####################################################################
# Argument Validation                                            ####

test_that(
  "importFileToRecord Argument Checking", 
  {
    # rcon is redcapApiConnection
    expect_error(importFileToRecord(rcon = mtcars, 
                                    file = "file1", 
                                    field = "field_name", 
                                    record = "30", 
                                    event = "event_1_arm_1", 
                                    repeat_instance = NULL), 
                 "Must inherit from class 'redcapApiConnection'")
    
    # file is a character(1)
    expect_error(importFileToRecord(rcon, 
                             file = 123, 
                             record = "30", 
                             field = "field_name", 
                             event = "event_1_arm_1"), 
                 "'file': Must be of type 'character'")
    
    expect_error(importFileToRecord(rcon, 
                             file = c("file1", "file2"), 
                             record = "30", 
                             field = "field_name", 
                             event = "event_1_arm_1"), 
                 "'file': Must have length 1")
    
    # record is not character (may also be numeric)
    expect_error(importFileToRecord(rcon,
                             record = TRUE,
                             field = "fieldname",
                             file = "filename", 
                             event = "event_1_arm_1"), 
                 "'record': Must be of type 'character'")
    
    # record is a character(1)
    expect_error(importFileToRecord(rcon, 
                             record = c("30", "31"), 
                             field = "field_name", 
                             file = "filename", 
                             event = "event_1_arm_1"), 
                 "'record': Must have length 1")
    
    # field is character(1)
    expect_error(importFileToRecord(rcon, 
                             record = 1,
                             field = 1, 
                             file = "filename", 
                             event = "event_1_arm_1"), 
                 "'field': Must be of type 'character'")
    
    expect_error(importFileToRecord(rcon,
                             record = 1, 
                             field = c("field1", "field2"), 
                             file = "filename", 
                             event = "event_1_arm_1"), 
                 "'field': Must have length 1")    
    
    # event is character(1)
    expect_error(importFileToRecord(rcon, 
                             record = 1, 
                             field = "field", 
                             file = "filename",
                             event = 1), 
                 "'event': Must be of type 'character'")
    
    expect_error(importFileToRecord(rcon, 
                             record = 1, 
                             field = "field", 
                             file = "filename",
                             event = c("event1", "event2")), 
                 "'event': Must have length 1") 
    
    
    # overwrite is logical(1)
    expect_error(importFileToRecord(rcon, 
                             record = 1, 
                             field = "field", 
                             file = "filename",
                             event = "event_1_arm_1",
                             overwrite = c(TRUE, FALSE)), 
                 "'overwrite': Must have length 1")
    
    expect_error(importFileToRecord(rcon, 
                             record = 1, 
                             field = "field", 
                             dir = "dir", 
                             file = "filename", 
                             event = "event_1_arm_1",
                             overwrite = "TRUE"), 
                 "'overwrite': Must be of type 'logical'")
    
    # repeat_instance is integerish(1)
    expect_error(importFileToRecord(rcon, 
                             record = 1, 
                             field = "file_import_field", 
                             file = "filename",
                             event = "event_1_arm_1",
                             repeat_instance = pi), 
                 "'repeat_instance': Must be of type 'integerish'")
    
    expect_error(importFileToRecord(rcon, 
                             record = 1, 
                             field = "file_import_field", 
                             file = "filename", 
                             event = "event_1_arm_1",
                             repeat_instance = c(1, 2)), 
                 "'repeat_instance': Must have length 1") 
  }
)

#####################################################################
# Functionality                                                  ####

orig_autonumber <- rcon$projectInformation()$record_autonumbering_enabled
orig_record <- unique(exportRecordsTyped(rcon, fields = "record_id")$record_id)
local_file <- test_path("testdata", "FileForImportExportTesting.txt")

test_that(
  "autonumber = TRUE, record = NULL, record_exists = FALSE", 
  {
    importProjectInformation(rcon, 
                             data.frame(record_autonumbering_enabled = 1))
    
    OrigRec <- exportRecordsTyped(rcon, fields = "record_id")
    
    importFileToRecord(rcon, 
                       file = local_file, 
                       record = NULL, 
                       field = "file_upload_test", 
                       event = "event_1_arm_1")
    
    NewRec <- exportRecordsTyped(rcon, fields = "record_id")
    expect_equal(nrow(NewRec) - 1, nrow(OrigRec))
  }
)

test_that(
  "autonumber = TRUE, record != NULL, record_exists = TRUE", 
  {
    importProjectInformation(rcon, 
                             data.frame(record_autonumbering_enabled = 1))
    
    OrigRec <- exportRecordsTyped(rcon, fields = "record_id")
    
    importFileToRecord(rcon, 
                       file = local_file, 
                       record = "20", 
                       field = "file_upload_test", 
                       event = "event_1_arm_1")
    
    NewRec <- exportRecordsTyped(rcon, fields = "record_id")
    expect_equal(nrow(NewRec), nrow(OrigRec))
  }
)

test_that(
  "autonumber = TRUE, record != NULL, record_exists = FALSE", 
  {
    importProjectInformation(rcon, 
                             data.frame(record_autonumbering_enabled = 1))
    
    OrigRec <- exportRecordsTyped(rcon, fields = "record_id")
    
    importFileToRecord(rcon, 
                       file = local_file, 
                       record = "100", 
                       field = "file_upload_test", 
                       event = "event_1_arm_1")
    
    NewRec <- exportRecordsTyped(rcon, fields = "record_id")
    expect_equal(nrow(NewRec) - 1, nrow(OrigRec))
  }
)

test_that(
  "autonumber = FALSE, record = NULL, record_exists = FALSE", 
  {
    importProjectInformation(rcon, 
                             data.frame(record_autonumbering_enabled = 0))

    expect_error(importFileToRecord(rcon, 
                                    file = local_file, 
                                    record = NULL, 
                                    field = "file_upload_test", 
                                    event = "event_1_arm_1"), 
                 "must be provided when autonumbering is not enabled")
  }
)

test_that(
  "autonumber = FALSE, record != NULL, record_exists = TRUE", 
  {
    importProjectInformation(rcon, 
                             data.frame(record_autonumbering_enabled = 0))
    
    OrigRec <- exportRecordsTyped(rcon, fields = "record_id")
    
    importFileToRecord(rcon, 
                       file = local_file, 
                       record = "20", 
                       field = "file_upload_test", 
                       event = "event_1_arm_1")
    
    NewRec <- exportRecordsTyped(rcon, fields = "record_id")
    expect_equal(nrow(NewRec), nrow(OrigRec))
  }
)

test_that(
  "autonumber = FALSE, record != NULL, record_exists = TRUE", 
  {
    importProjectInformation(rcon, 
                             data.frame(record_autonumbering_enabled = 0))
    
    OrigRec <- exportRecordsTyped(rcon, fields = "record_id")
    
    importFileToRecord(rcon, 
                       file = local_file, 
                       record = "100", 
                       field = "file_upload_test", 
                       event = "event_1_arm_1")
    
    NewRec <- exportRecordsTyped(rcon, fields = "record_id")
    expect_equal(nrow(NewRec) - 1, nrow(OrigRec))
  }
)

Rec <- exportRecordsTyped(rcon, fields = "record_id")
rec_to_remove <- Rec$record_id[!Rec$record_id %in% orig_record]

deleteRecords(rcon, records = rec_to_remove)

importProjectInformation(rcon, 
                         data.frame(record_autonumbering_enabled = orig_autonumber))

rm(list = c("orig_record", "rec_to_remove", "orig_autonumber"))
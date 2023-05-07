context("export/import/delete Files Functionality")

#####################################################################
# Establish a project for testing.                               ####
# It will need one record in order to have a place to store a record

purgeProject(rcon, 
             purge_all = TRUE)

load(test_path("testdata", "RedcapProject_BasicData.Rdata"))
rcon$flush_all()
restoreProject(RedcapProject_BasicData, 
               rcon)
importRecords(rcon, 
              data.frame(record_id = "1"))


local_file <- test_path("testdata", "FileForImportExportTesting.txt")


#####################################################################
# Non-longitudinal project                                       ####

test_that(
  "import, export, and delete a file in a classic project", 
  {
    expect_message(importFiles(rcon, 
                               file = local_file, 
                               record = "1", 
                               field = "file_upload_test", 
                               overwrite = FALSE, 
                               repeat_instance = NULL), 
                   "The file was successfully uploaded")
    
    temp_dir <- tempdir()
    target_file <- "30-event_1_arm_1-FileForImportExportTesting.txt"
    
    # Export the file
    expect_message(
      exportFiles(rcon,
                  record = "1",
                  field = "file_upload_test",
                  event = NULL,
                  dir = temp_dir,
                  filePrefix = TRUE),
      "The file was saved to '.+1-FileForImportExportTesting.txt"
    )
    
    # And now import it again with an overwrite
    expect_message(
      importFiles(rcon,
                  file = local_file,
                  record = "1",
                  field = "file_upload_test",
                  event = NULL,
                  overwrite = TRUE,
                  repeat_instance = NULL),
      "The file was successfully uploaded"
    )
    
    # Attempt an import without overwrite = TRUE. Results in error
    expect_error(
      importFiles(rcon,
                  file = local_file,
                  record = "1",
                  field = "file_upload_test",
                  event = NULL,
                  overwrite = FALSE,
                  repeat_instance = NULL),
      "A file exists and overwrite [=] FALSE"
    )
    
    # Delete the file
    expect_message(
      deleteFiles(rcon,
                  record = "1",
                  field = "file_upload_test",
                  event = NULL),
      "The file was successfully deleted"
    )
  }
)

#####################################################################
# Longitudinal project                                           ####

NewArm <- data.frame(arm_num = c(1, 2), 
                     name = c("arm_1", "arm_2"))
NewEvent <- data.frame(event_name = c("event_1", "event_1"), 
                       arm_num = c("1", "2"), 
                       unique_event_name = c("event_1_arm_1", 
                                             "event_1_arm_2"))
NewMapping <- data.frame(arm_num = c(1, 1), 
                         unique_event_name = c("event_1_arm_1",
                                               "event_1_arm_1"),
                         form = c("record_id", "files_notes_descriptions"))

importProjectInformation(rcon, data.frame(is_longitudinal = 1))
importArms(rcon, NewArm)
importEvents(rcon, NewEvent)
rcon$flush_all()
importMappings(rcon, NewMapping)

test_that(
  "import, export, and delete a file in a longitudinal project", 
  {
    expect_message(
      importFiles(rcon,
                  file = local_file, 
                  record = "1", 
                  field = "file_upload_test", 
                  event = "event_1_arm_1", 
                  overwrite = FALSE, 
                  repeat_instance = NULL), 
      "The file was successfully uploaded"
    )
    
    
    temp_dir <- tempdir()
    
    # Export the file
    expect_message(
      exportFiles(rcon,
                  record = "1",
                  field = "file_upload_test",
                  event = "event_1_arm_1",
                  dir = temp_dir,
                  filePrefix = TRUE),
      "The file was saved to '.+1-event_1_arm_1-FileForImportExportTesting.txt"
    )
    
    # And now import it again with an overwrite
    expect_message(
      importFiles(rcon,
                  file = local_file,
                  record = "1",
                  field = "file_upload_test",
                  event = "event_1_arm_1",
                  overwrite = TRUE,
                  repeat_instance = NULL),
      "The file was successfully uploaded"
    )
    
    # Attempt an import without overwrite = TRUE. Results in error
    expect_error(
      importFiles(rcon,
                  file = local_file,
                  record = "1",
                  field = "file_upload_test",
                  event = "event_1_arm_1",
                  overwrite = FALSE,
                  repeat_instance = NULL),
      "A file exists and overwrite [=] FALSE"
    )
    
    # Delete the file
    expect_message(
      deleteFiles(rcon,
                  record = "1",
                  field = "file_upload_test",
                  event = "event_1_arm_1"),
      "The file was successfully deleted"
    )
  }
)

#####################################################################
# Repeating Instrument                                           ####

deleteRecords(rcon, records = "1")

NewRepeat <- data.frame(event_name = "event_1_arm_1", 
                        form_name = "files_notes_descriptions")
importRepeatingInstrumentsEvents(rcon, 
                                 NewRepeat)
rcon$flush_all()

importRecords(rcon, 
              data.frame(record_id = "1", 
                         redcap_event_name = "event_1_arm_1", 
                         notes_test = "some notes", 
                         redcap_repeat_instrument = "files_notes_descriptions", 
                         redcap_repeat_instance = "1"))

test_that(
  "import, export, and delete a file in a longitudinal project", 
  {
    expect_message(
      importFiles(rcon,
                  file = local_file, 
                  record = "1", 
                  field = "file_upload_test", 
                  event = "event_1_arm_1", 
                  overwrite = FALSE, 
                  repeat_instance = 1), 
      "The file was successfully uploaded"
    )
    
    
    temp_dir <- tempdir()
    
    # Export the file
    expect_message(
      exportFiles(rcon,
                  record = "1",
                  field = "file_upload_test",
                  event = "event_1_arm_1",
                  repeat_instance = 1,
                  dir = temp_dir,
                  filePrefix = TRUE),
      "The file was saved to '.+1-event_1_arm_1-FileForImportExportTesting.txt"
    )
    
    # And now import it again with an overwrite
    expect_message(
      importFiles(rcon,
                  file = local_file,
                  record = "1",
                  field = "file_upload_test",
                  event = "event_1_arm_1",
                  overwrite = TRUE,
                  repeat_instance = 1),
      "The file was successfully uploaded"
    )
    
    # Attempt an import without overwrite = TRUE. Results in error
    expect_error(
      importFiles(rcon,
                  file = local_file,
                  record = "1",
                  field = "file_upload_test",
                  event = "event_1_arm_1",
                  overwrite = FALSE,
                  repeat_instance = 1),
      "A file exists and overwrite [=] FALSE"
    )
    
    # Delete the file
    expect_message(
      deleteFiles(rcon,
                  record = "1",
                  field = "file_upload_test",
                  event = "event_1_arm_1", 
                  repeate_instance = 1),
      "The file was successfully deleted"
    )
  }
)

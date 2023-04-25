context("export/import/delete Files Functionality")

# TestRedcapAPI project was restored in test-04-fieldnames.R
rcon <- redcapConnection(url, API_KEY)

local_file <- test_path("testdata", "FileForImportExportTesting.txt")

test_that(
  "import, export, and delete a file in a longitudinal project", 
  {
    expect_message(
      importFiles(rcon,
                  file = local_file, 
                  record = "30", 
                  field = "file_import_field", 
                  event = "event_1_arm_1", 
                  overwrite = FALSE, 
                  repeat_instance = NULL), 
      "The file was successfully uploaded"
    )
    
    
    temp_dir <- tempdir()
    target_file <- "30-event_1_arm_1-FileForImportExportTesting.txt"
    
    # Export the file
    expect_message(
      exportFiles(rcon,
                  record = "30",
                  field = "file_import_field",
                  event = "event_1_arm_1",
                  dir = temp_dir,
                  filePrefix = TRUE),
      "The file was saved to '.+30-event_1_arm_1-FileForImportExportTesting.txt"
    )
    
    # And now import it again with an overwrite
    expect_message(
      importFiles(rcon,
                  file = local_file,
                  record = "30",
                  field = "file_import_field",
                  event = "event_1_arm_1",
                  overwrite = TRUE,
                  repeat_instance = NULL),
      "The file was successfully uploaded"
    )
    
    # Attempt an import without overwrite = TRUE. Results in error
    expect_error(
      importFiles(rcon,
                  file = local_file,
                  record = "30",
                  field = "file_import_field",
                  event = "event_1_arm_1",
                  overwrite = FALSE,
                  repeat_instance = NULL),
      "A file exists and overwrite [=] FALSE"
    )
    
    # Delete the file
    expect_message(
      deleteFiles(rcon,
                  record = "30",
                  field = "file_import_field",
                  event = "event_1_arm_1"),
      "The file was successfully deleted"
    )
    
  }
)

# FIXME: We don't have any tests for a classic project. There's no reason
#        to think it wouldn't work, but it's something we might consider.

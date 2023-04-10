context("export/import/delete Files Functionality")

rcon <- redcapConnection(url, API_KEY)

test_that(
  "export, delete, and import a file in a longitudinal project", 
  {
    local_reproducible_output(width = 200)
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
    
    # Delete the file
    expect_message(
      deleteFiles(rcon, 
                  record = "30", 
                  field = "file_import_field", 
                  event = "event_1_arm_1"), 
      "The file was successfully deleted"
    )
    
    # Reimport the file
    file_no_prefix <- sub("(^.+)(FileFor.+$)", "\\2", target_file)
    file.rename(file.path(temp_dir, target_file), 
                file.path(temp_dir, file_no_prefix))
    expect_message(
      importFiles(rcon,
                  file = file.path(temp_dir, file_no_prefix), 
                  record = "30", 
                  field = "file_import_field", 
                  event = "event_1_arm_1", 
                  overwrite = FALSE, 
                  repeat_instance = NULL), 
      "The file was successfully uploaded"
    )
    
    # And now import it again with an overwrite
    expect_message(
      importFiles(rcon,
                  file = file.path(temp_dir, file_no_prefix), 
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
                  file = file.path(temp_dir, file_no_prefix), 
                  record = "30", 
                  field = "file_import_field", 
                  event = "event_1_arm_1", 
                  overwrite = FALSE, 
                  repeat_instance = NULL), 
      "A file exists and overwrite [=] FALSE"
    )
  }
)

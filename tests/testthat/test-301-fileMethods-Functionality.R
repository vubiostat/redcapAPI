context("export/import/delete Files Functionality")

local_file <- test_path("testdata", "FileForImportExportTesting.txt")

test_that(
  "import, export, and delete a file in a longitudinal project",
  {
    local_reproducible_output(width = 200)
    
    expect_true(
      importFiles(rcon,
                  file = local_file,
                  record = "1",
                  field = "file_upload_test",
                  event = "event_1_arm_1",
                  overwrite = FALSE,
                  repeat_instance = NULL)
    )


    temp_dir <- tempdir()

    # Export the file
    save_to <- exportFiles(rcon,
                           record = "1",
                           field = "file_upload_test",
                           event = "event_1_arm_1",
                           dir = temp_dir,
                           file_prefix = TRUE)
    expect_true(grepl(".+1-event_1_arm_1-FileForImportExportTesting.txt", 
                      save_to))

    # And now import it again with an overwrite
    expect_true(
      importFiles(rcon,
                  file = local_file,
                  record = "1",
                  field = "file_upload_test",
                  event = "event_1_arm_1",
                  overwrite = TRUE,
                  repeat_instance = NULL)
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
    expect_true(
      deleteFiles(rcon,
                  record = "1",
                  field = "file_upload_test",
                  event = "event_1_arm_1")
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

importRecords(rcon,
              data.frame(record_id = "1",
                         redcap_event_name = "event_1_arm_1",
                         notes_test = "some notes",
                         redcap_repeat_instrument = "files_notes_descriptions",
                         redcap_repeat_instance = "1"))

test_that(
  "import, export, and delete a file in a longitudinal project",
  {
    local_reproducible_output(width = 200)
    expect_true(
      importFiles(rcon,
                  file = local_file,
                  record = "1",
                  field = "file_upload_test",
                  event = "event_1_arm_1",
                  overwrite = FALSE,
                  repeat_instance = 1)
    )


    temp_dir <- tempdir()
    
    local_reproducible_output(width = 200)
   
    expect_error(exportFiles(rcon,
                             record = "1",
                             field = "file_upload_test",
                             event = "event_1_arm_1",
                             repeat_instance = 1,
                             dir = temp_dir,
                             file_prefix = TRUE,
                             config = list(1)), 
                 "'config': Must have names")
    expect_error(exportFiles(rcon,
                             record = "1",
                             field = "file_upload_test",
                             event = "event_1_arm_1",
                             repeat_instance = 1,
                             dir = temp_dir,
                             file_prefix = TRUE, 
                             config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportFiles(rcon,
                             record = "1",
                             field = "file_upload_test",
                             event = "event_1_arm_1",
                             repeat_instance = 1,
                             dir = temp_dir,
                             file_prefix = TRUE, 
                             api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportFiles(rcon,
                             record = "1",
                             field = "file_upload_test",
                             event = "event_1_arm_1",
                             repeat_instance = 1,
                             dir = temp_dir,
                             file_prefix = TRUE,
                             api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")

    # Export the file
    save_to <- exportFiles(rcon,
                           record = "1",
                           field = "file_upload_test",
                           event = "event_1_arm_1",
                           repeat_instance = 1,
                           dir = temp_dir,
                           file_prefix = TRUE)
    expect_true(
      grepl(".+1-event_1_arm_1-FileForImportExportTesting.txt", 
            save_to)
    )
    
    local_reproducible_output(width = 200)

    expect_error(importFiles(rcon,
                             file = local_file,
                             record = "1",
                             field = "file_upload_test",
                             event = "event_1_arm_1",
                             overwrite = TRUE,
                             repeat_instance = 1,
                             config = list(1)), 
                 "'config': Must have names")
    expect_error(importFiles(rcon,
                             file = local_file,
                             record = "1",
                             field = "file_upload_test",
                             event = "event_1_arm_1",
                             overwrite = TRUE,
                             repeat_instance = 1,
                             config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(importFiles(rcon,
                             file = local_file,
                             record = "1",
                             field = "file_upload_test",
                             event = "event_1_arm_1",
                             overwrite = TRUE,
                             repeat_instance = 1,
                             api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(importFiles(rcon,
                             file = local_file,
                             record = "1",
                             field = "file_upload_test",
                             event = "event_1_arm_1",
                             overwrite = TRUE,
                             repeat_instance = 1,
                             api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")

    # And now import it again with an overwrite
    expect_true(
      importFiles(rcon,
                  file = local_file,
                  record = "1",
                  field = "file_upload_test",
                  event = "event_1_arm_1",
                  overwrite = TRUE,
                  repeat_instance = 1)
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

    local_reproducible_output(width = 200)
    expect_error(deleteFiles(rcon, 
                             record = "1",
                             field = "file_upload_test",
                             event = "event_1_arm_1",
                             repeat_instance = 1, 
                             config = list(1)), 
                 "'config': Must have names")
    expect_error(deleteFiles(rcon, 
                             record = "1",
                             field = "file_upload_test",
                             event = "event_1_arm_1",
                             repeat_instance = 1, 
                             config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(deleteFiles(rcon, 
                             record = "1",
                             field = "file_upload_test",
                             event = "event_1_arm_1",
                             repeat_instance = 1, 
                             api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(deleteFiles(rcon, 
                             record = "1",
                             field = "file_upload_test",
                             event = "event_1_arm_1",
                             repeat_instance = 1,
                             api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
    
    # Delete the file
    expect_true(
      deleteFiles(rcon,
                  record = "1",
                  field = "file_upload_test",
                  event = "event_1_arm_1",
                  repeat_instance = 1)
    )
    
    lapply(list.files(temp_dir), 
           unlink)
  }
)

test_that(
  "Files imported/exported from repeat instances > 1", 
  {
    temp_file <- tempfile(fileext = ".txt")
    file.copy(local_file, temp_file)
    
    expect_true(importFiles(rcon, 
                            file = local_file, 
                            record = "1", 
                            field = "file_upload_test", 
                            event = "event_1_arm_1", 
                            repeat_instance = 1))
    
    expect_true(importFiles(rcon, 
                            file = temp_file, 
                            record = "1", 
                            field = "file_upload_test", 
                            event = "event_1_arm_1", 
                            repeat_instance = 2))
    
    target_dir <- tempdir()
    
    file1 <- exportFiles(rcon, 
                         record = "1", 
                         field = "file_upload_test", 
                         event = "event_1_arm_1", 
                         dir = target_dir, 
                         repeat_instance = 1)
    expect_true(file.exists(file1))
    expect_true(grepl("FileForImportExportTesting.txt", file1))
    
    file2 <- exportFiles(rcon, 
                         record = "1", 
                         field = "file_upload_test", 
                         event = "event_1_arm_1", 
                         dir = target_dir, 
                         repeat_instance = 2)
    
    expect_true(file.exists(file2))
    expect_true(grepl(basename(temp_file), file2))
    
    expect_true(deleteFiles(rcon, 
                            record = "1", 
                            field = "file_upload_test", 
                            event = "event_1_arm_1", 
                            repeat_instance = 1))
    
    expect_true(deleteFiles(rcon, 
                            record = "1", 
                            field = "file_upload_test", 
                            event = "event_1_arm_1", 
                            repeat_instance = 2))
    
    # clean up
    unlink(temp_file)
  }
)

context("export/import/delete Files Functionality")

local_file <- test_path("testdata", "FileForImportExportTesting.txt")

test_that(
  "import, export, and delete a file in a longitudinal project",
  {
    local_reproducible_output(width = 200)
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
                  file_prefix = TRUE),
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
    local_reproducible_output(width = 200)
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
                  file_prefix = TRUE),
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

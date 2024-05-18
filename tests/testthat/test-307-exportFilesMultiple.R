#####################################################################
# Argument Validation                                            ####

test_that(
  "exportFilesMultiple Argument Checking", 
  {
    local_reproducible_output(width = 200)
    # rcon is redcapApiConnection
    expect_error(exportFilesMultiple(rcon = mtcars), 
                 "no applicable method for 'exportFilesMultiple'")
    
    # record is not character (may also be numeric)
    expect_error(exportFilesMultiple(rcon,
                                     record = TRUE, 
                                     field = "field_name", 
                                     dir = "dir"), 
                 "'record': Must be of type 'character'")
    
    # field is character
    expect_error(exportFilesMultiple(rcon, 
                                     record = 1,
                                     field = 1, 
                                     dir = "dir"), 
                 "'field': Must be of type 'character'")
    
    # event is character
    expect_error(exportFilesMultiple(rcon, 
                                             record = 1, 
                                             field = "field", 
                                             dir = "dir",
                                             event = 1), 
                 "'event': Must be of type 'character'")
    
    # dir is character(1)
    expect_error(exportFilesMultiple(rcon, 
                                             record = 1, 
                                             field = "field", 
                                             dir = 123,
                                             event = NULL), 
                 "'dir': Must be of type 'character'")
    
    expect_error(exportFilesMultiple(rcon, 
                                     record = 1, 
                                     field = "field", 
                                     dir = c("dir1", "dir2"),
                                     event = NULL), 
                 "'dir': Must have length 1") 
    
    # file_prefix is logical(1)
    expect_error(exportFilesMultiple(rcon, 
                                     record = 1, 
                                     field = "field", 
                                     dir = "dir", 
                                     event = NULL, 
                                     file_prefix = c(TRUE, FALSE)), 
                 "'file_prefix': Must have length 1")
    
    expect_error(exportFilesMultiple(rcon, 
                                     record = 1, 
                                     field = "field", 
                                     dir = "dir", 
                                     event = NULL, 
                                     file_prefix = "TRUE"), 
                 "'file_prefix': Must be of type 'logical'")
    
    # repeat_instance is integerish
    expect_error(exportFilesMultiple(rcon, 
                                     record = 1, 
                                     field = "file_import_field", 
                                     dir = "dir", 
                                     event = NULL,
                                     repeat_instance = pi), 
                 "'repeat_instance': Must be of type 'integerish'")
    
    # quiet is logical(1)
    expect_error(exportFilesMultiple(rcon, 
                                     record = 1, 
                                     field = "field", 
                                     dir = "dir", 
                                     event = NULL, 
                                     quiet = c(TRUE, FALSE)), 
                 "'quiet': Must have length 1")
    
    expect_error(exportFilesMultiple(rcon, 
                                     record = 1, 
                                     field = "field", 
                                     dir = "dir", 
                                     event = NULL, 
                                     quiet = "TRUE"), 
                 "'quiet': Must be of type 'logical'")
  }
)

#####################################################################
# Functionality                                                  ####

local_file <- test_path("testdata", "FileForImportExportTesting.txt")

test_that(
  "Export multiple files", 
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
    
    Files <- exportFilesMultiple(rcon, 
                                 record = "1", 
                                 field = "file_upload_test", 
                                 event = "event_1_arm_1", 
                                 repeat_instance = 1:2, 
                                 dir = target_dir)
    
    expect_data_frame(Files, 
                      nrows = 2)
    expect_equal(Files$is_exported, rep(TRUE, 2))
    
    # Force an error for a non-existent file
    
    Files <- exportFilesMultiple(rcon, 
                                 record = "1", 
                                 field = "file_upload_test", 
                                 event = "event_1_arm_1", 
                                 repeat_instance = 1:3, 
                                 dir = target_dir)
    
    expect_data_frame(Files, 
                      nrows = 3)
    expect_equal(Files$is_exported, 
                 c(TRUE, TRUE, FALSE))
    
    # Force an error for a bad argument to exportFiles
    
    FilesError <- exportFilesMultiple(rcon, 
                                      record = "1", 
                                      field = "record_id", 
                                      event = "event_1_arm_1", 
                                      repeat_instance = 1:2, 
                                      dir = target_dir)
    
    expect_data_frame(FilesError, 
                      nrows = 2)
    expect_true(grepl("'record_id' is not of field type 'file'", FilesError$error[1]))
    
    
    # Issue message when quiet = FALSE
    
    FilesError <- 
      expect_message(exportFilesMultiple(rcon, 
                                         record = "1", 
                                         field = "record_id", 
                                         event = "event_1_arm_1", 
                                         repeat_instance = 1:2, 
                                         dir = target_dir, 
                                         quiet = FALSE), 
                     "is not of field type")
    
    # clean up
    unlink(temp_file)
    lapply(Files$saved_to, 
           unlink)
  }
)

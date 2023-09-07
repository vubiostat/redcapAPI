context("reconstituteFileFromExport.R")


local_file <- test_path("testdata", "FileForImportExportTesting.txt")

importFiles(rcon, 
            file = local_file, 
            record = "1", 
            field = "file_upload_test", 
            event = "event_1_arm_1")

RESPONSE <- makeApiCall(rcon, 
                        body = list(content = 'file', 
                                    action = 'export', 
                                    record = '1', 
                                    field = 'file_upload_test', 
                                    event = 'event_1_arm_1'))

EXISTENT_DIR <- tempdir()

test_that(
  "File is saved to the directory", 
  {
    SavedFile <- reconstituteFileFromExport(RESPONSE,  
                                            dir = EXISTENT_DIR)
    filename <- gsub(pattern = "(^[[:print:]]+; name=|\")", 
                     replacement = "", 
                     x = RESPONSE$headers$'content-type')
    filename <- sub(pattern = ";charset.+$", 
                    replacement = "", 
                    x = filename)
    expect_equal(SavedFile$filename, 
                 "FileForImportExportTesting.txt")
    expect_true(file.exists(file.path(EXISTENT_DIR, filename)))
    
    unlink(file.path(EXISTENT_DIR, filename))
  }
)

test_that(
  "New directory is created and file is saved to that directory", 
  {
    NON_EXISTENT_DIR <- file.path(tempdir(), "this_dir_no_findy")
    SavedFile <- reconstituteFileFromExport(RESPONSE,  
                                            dir = NON_EXISTENT_DIR, 
                                            dir_create = TRUE)
    filename <- gsub(pattern = "(^[[:print:]]+; name=|\")", 
                     replacement = "", 
                     x = RESPONSE$headers$'content-type')
    filename <- sub(pattern = ";charset.+$", 
                    replacement = "", 
                    x = filename)
    expect_equal(SavedFile$filename, 
                 "FileForImportExportTesting.txt")
    expect_true(file.exists(file.path(NON_EXISTENT_DIR, filename)))
    
    unlink(file.path(NON_EXISTENT_DIR, filename))
    unlink(NON_EXISTENT_DIR)
  }
)

test_that(
  "Return an error if rcon is not a redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(reconstituteFileFromExport(mtcars, 
                                            dir = EXISTENT_DIR, 
                                            dir_create = FALSE), 
                 "'response': Must inherit from class 'response'")
  }
)

test_that(
  "Return an error if dir is not character(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(reconstituteFileFromExport(RESPONSE, 
                                            dir = c("dir1", "dir2"), 
                                            dir_create = FALSE), 
                 "'dir'[:] Must have length 1")
    expect_error(reconstituteFileFromExport(RESPONSE, 
                                            dir = 123, 
                                            dir_create = FALSE), 
                 "'dir'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if dir_create is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(reconstituteFileFromExport(RESPONSE, 
                                            dir = EXISTENT_DIR, 
                                            dir_create = c(FALSE, TRUE)), 
                 "'dir_create'[:] Must have length 1")
    expect_error(reconstituteFileFromExport(RESPONSE, 
                                            dir = EXISTENT_DIR, 
                                            dir_create = "FALSE"), 
                 "'dir_create'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if dir doesn't exist and dir_create = FALSE", 
  {
    local_reproducible_output(width = 200)
    UNKNOWN_DIR <- file.path(EXISTENT_DIR, "ImNotHere")
    expect_error(reconstituteFileFromExport(RESPONSE, 
                                            dir = UNKNOWN_DIR, 
                                            dir_create = FALSE), 
                 "'dir'[:] Directory '.+ImNotHere' does not exist")
  }
)

test_that(
  "Return an error if prefix is not character(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(reconstituteFileFromExport(RESPONSE, 
                                            dir = EXISTENT_DIR, 
                                            dir_create = FALSE, 
                                            file_prefix = c("123", "123")), 
                 "'file_prefix'[:] Must have length 1")
    expect_error(reconstituteFileFromExport(RESPONSE, 
                                            dir = EXISTENT_DIR, 
                                            dir_create = FALSE, 
                                            file_prefix = 123), 
                 "'file_prefix'[:] Must be of type 'character'")
  }
)


# Clean up

deleteFiles(rcon, record = "1", 
            field = "file_upload_test", 
            event = "event_1_arm_1")

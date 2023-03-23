context("exportFileFromRepository.R")

rcon <- redcapConnection(url= url, 
                         token = API_KEY)

EXISTENT_DIR <- tempdir()

NON_EXISTENT_DIR <- file.path(EXISTENT_DIR, "NotAFolderYet")

# FIXME: A file should be loaded into the repository to get the doc_id for testing.
FileRepo <- exportFileRepositoryListing(rcon, 
                                        recursive = TRUE)
DOC_ID <- FileRepo$doc_id[!is.na(FileRepo$doc_id)][1]

FILENAME <- FileRepo$name[FileRepo$doc_id == DOC_ID]

test_that(
  "File is saved to the directory", 
  {
    SavedFile <- exportFromFileRepository(rcon,  
                                          doc_id = DOC_ID, 
                                          dir = EXISTENT_DIR)
    expect_equal(SavedFile, 
                 data.frame(directory = EXISTENT_DIR, 
                            filename = FILENAME))
    expect_true(file.exists(file.path(EXISTENT_DIR, FILENAME)))
    
    unlink(file.path(EXISTENT_DIR, FILENAME))
  }
)

test_that(
  "New directory is created and file is saved to that directory", 
  {
    SavedFile <- exportFromFileRepository(rcon, 
                                          doc_id = DOC_ID,
                                          dir = NON_EXISTENT_DIR, 
                                          dir_create = TRUE)
    
    expect_equal(SavedFile, 
                 data.frame(directory = NON_EXISTENT_DIR, 
                            filename = FILENAME))
    expect_true(file.exists(file.path(NON_EXISTENT_DIR, FILENAME)))
    
    unlink(file.path(NON_EXISTENT_DIR, FILENAME))
    unlink(NON_EXISTENT_DIR)
  }
)

test_that(
  "Return an error if rcon is not a redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFromFileRepository(mtcars, 
                                          doc_id = DOC_ID,
                                          dir = EXISTENT_DIR, 
                                          dir_create = FALSE), 
                 "no applicable method for 'exportFromFileRepository'")
  }
)

test_that(
  "Return an error if doc_id is not numeric(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFromFileRepository(rcon,
                                          doc_id = c(DOC_ID, 1),
                                          dir = EXISTENT_DIR, 
                                          dir_create = FALSE), 
                 "'doc_id'[:] Must have length 1")
    expect_error(exportFromFileRepository(rcon, 
                                          doc_id = "123",
                                          dir = EXISTENT_DIR, 
                                          dir_create = FALSE), 
                 "'doc_id'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error if dir is not character(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFromFileRepository(rcon,
                                          doc_id = DOC_ID,
                                          dir = c("dir1", "dir2"), 
                                          dir_create = FALSE), 
                 "'dir'[:] Must have length 1")
    expect_error(exportFromFileRepository(rcon, 
                                          doc_id = DOC_ID,
                                          dir = 123, 
                                          dir_create = FALSE), 
                 "'dir'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if dir_create is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFromFileRepository(rcon, 
                                          doc_id = DOC_ID,
                                          dir = EXISTENT_DIR, 
                                          dir_create = c(FALSE, TRUE)), 
                 "'dir_create'[:] Must have length 1")
    expect_error(exportFromFileRepository(rcon,
                                          doc_id = DOC_ID,
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
    
    expect_error(exportFromFileRepository(rcon, 
                                          doc_id = DOC_ID,
                                          dir = UNKNOWN_DIR, 
                                          dir_create = FALSE), 
                 "'dir'[:] Directory '.+ImNotHere' does not exist")
  }
)

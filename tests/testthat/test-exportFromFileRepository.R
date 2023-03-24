context("exportFileFromRepository.R")

rcon <- redcapConnection(url= url, 
                         token = API_KEY)

# Only the argument validations are tested in this file. 
# See test-fileRepositoryTests.R for functional tests

test_that(
  "Return an error if rcon is not a redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFromFileRepository(mtcars, 
                                          doc_id = 123,
                                          dir = "directory", 
                                          dir_create = FALSE), 
                 "no applicable method for 'exportFromFileRepository'")
  }
)

test_that(
  "Return an error if doc_id is not numeric(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFromFileRepository(rcon,
                                          doc_id = c(123, 234),
                                          dir = "directory", 
                                          dir_create = FALSE), 
                 "'doc_id'[:] Must have length 1")
    expect_error(exportFromFileRepository(rcon, 
                                          doc_id = "123",
                                          dir = "directory", 
                                          dir_create = FALSE), 
                 "'doc_id'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error if dir is not character(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFromFileRepository(rcon,
                                          doc_id = 123,
                                          dir = c("dir1", "dir2"), 
                                          dir_create = FALSE), 
                 "'dir'[:] Must have length 1")
    expect_error(exportFromFileRepository(rcon, 
                                          doc_id = 123,
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
                                          doc_id = 123,
                                          dir = "directory", 
                                          dir_create = c(FALSE, TRUE)), 
                 "'dir_create'[:] Must have length 1")
    expect_error(exportFromFileRepository(rcon,
                                          doc_id = 123,
                                          dir = "directory", 
                                          dir_create = "FALSE"), 
                 "'dir_create'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if dir doesn't exist and dir_create = FALSE", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(exportFromFileRepository(rcon, 
                                          doc_id = 123,
                                          dir = "ImNotHere", 
                                          dir_create = FALSE), 
                 "'dir'[:] Directory 'ImNotHere' does not exist")
  }
)

context("deleteFileRepository.R")

# Only the validations are tested in this file. 
# See test-exportImportDeleteFileRepository.R for functional tests

rcon <- redcapConnection(url = url, 
                         token = API_KEY)

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteFileRepository(mtcars, 
                                      folder_id = 123), 
                 "no applicable method for 'deleteFileRepository'")
  }
)

test_that(
  "Return an error if folder_id is not integerish(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = c(1, 2)), 
                 "Variable 'folder_id'[:] Must have length 1")
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = "1"), 
                 "Variable 'folder_id'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error if recursive is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = 123, 
                                      recursive = c(TRUE, FALSE)), 
                 "Variable 'recursive'[:] Must have length 1")
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = 123, 
                                      recursive = "TRUE"), 
                 "Variable 'recursive'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if refresh is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = 123, 
                                      refresh = c(TRUE, FALSE)), 
                 "Variable 'refresh'[:] Must have length 1")
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = 123, 
                                      refresh = "TRUE"), 
                 "Variable 'refresh'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error when confirm is not ask, no, or yes", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = 123,
                                      confirm = "different"),
                 "'confirm'[:] Must be element of set")
  }
)
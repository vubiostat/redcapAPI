context("test-importFileRepository")

rcon <- redcapConnection(url = url, 
                         token = API_KEY)

# Only the validations are tested in this file. 
# See test-exportImportDeleteFileRepository.R for functional tests

test_that(
  "Return an error when rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(importFileRepository(mtcars, 
                                      dir = "folder"), 
                 "no applicable method for 'importFileRepository'")
  }
)

test_that(
  "Return an error when dir is not character(1) or directory doesn't exist", 
  {
    local_reproducible_output(width = 200)
    expect_error(importFileRepository(rcon, 
                                      dir = c("folder1", "folder2")), 
                 "Variable 'dir'[:] Must have length 1")
    expect_error(importFileRepository(rcon, 
                                      dir = 123), 
                 "Variable 'dir'[:] Must be of type 'character'")
    expect_error(importFileRepository(rcon, 
                                      dir = "FileDoesntExist"), 
                 "'dir'[:] Directory 'FileDoesntExist' does not exist")
  }
)


test_that(
  "Return an error when folder_id is not numeric(0/1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importFileRepository(rcon, 
                                      dir = tempdir(), 
                                      folder_id = c(123, 234)), 
                 "'folder_id'[:] Must have length <= 1")
    expect_error(importFileRepository(rcon, 
                                      dir = tempdir(), 
                                      folder_id = "123"), 
                 "'folder_id'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error when dag_id is not numeric(0/1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importFileRepository(rcon, 
                                      dir = tempdir(), 
                                      dag_id = c(123, 234)), 
                 "'dag_id'[:] Must have length <= 1")
    expect_error(importFileRepository(rcon, 
                                      dir = tempdir(), 
                                      dag_id = "123"), 
                 "'dag_id'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error when role_id is not numeric(0/1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importFileRepository(rcon, 
                                      dir = tempdir(), 
                                      role_id = c(123, 234)), 
                 "'role_id'[:] Must have length <= 1")
    expect_error(importFileRepository(rcon, 
                                      dir = tempdir(), 
                                      role_id = "123"), 
                 "'role_id'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error when recursive is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importFileRepository(rcon, 
                                      dir = tempdir(), 
                                      recursive = c(TRUE, FALSE)), 
                 "'recursive'[:] Must have length 1")
    expect_error(importFileRepository(rcon, 
                                      dir = tempdir(), 
                                      recursive = "TRUE"), 
                 "'recursive'[:] Must be of type 'logical'") 
  }
)

test_that(
  "Return an error when refresh is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importFileRepository(rcon, 
                                      dir = tempdir(), 
                                      refresh = c(TRUE, FALSE)), 
                 "'refresh'[:] Must have length 1")
    expect_error(importFileRepository(rcon, 
                                      dir = tempdir(), 
                                      refresh = "TRUE"), 
                 "'refresh'[:] Must be of type 'logical'") 
  }
)

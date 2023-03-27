context("exportFileRepository.R")

rcon <- redcapConnection(url = url, 
                         token = API_KEY)

# Only the validations are tested in this file. 
# See test-exportImportDeleteFileRepository.R for functional tests

test_that(
  "Return an error when rcon is not a redcapConnection",
  {
    local_reproducible_output(width = 200)
    expect_error(exportFileRepository(mtcars), 
                 "no applicable method for 'exportFileRepository'")
  }
)

test_that(
  "Return an error when folder_id is not numeric(0/1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportFileRepository(rcon, 
                                      folder_id = c(1, 2)), 
                 "Variable 'folder_id'[:] Must have length [<][=] 1")
    
    expect_error(exportFileRepository(rcon, 
                                      folder_id = "1"), 
                 "Variable 'folder_id'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error when dir is not character(1) or doesn't exist", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFileRepository(rcon, 
                                      dir = c("dir1", "dir2")), 
                 "Variable 'dir'[:] Must have length 1")
    
    expect_error(exportFileRepository(rcon, 
                                      dir = 1), 
                 "Variable 'dir'[:] Must be of type 'character'")
    expect_error(exportFileRepository(rcon, 
                                      dir = "DirectoryNotHere"), 
                 "Variable 'dir'[:] Directory 'DirectoryNotHere' does not exist.")
  }
)

test_that(
  "Return an error dir_create is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFileRepository(rcon, 
                                      dir_create = c(TRUE, FALSE)), 
                 "Variable 'dir_create'[:] Must have length 1")
    expect_error(exportFileRepository(rcon, 
                                      dir_create = "TRUE"), 
                 "Variable 'dir_create': Must be of type 'logical'")
  }
)

test_that(
  "Return an error when recursive is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFileRepository(rcon, 
                                      recursive = c(TRUE, FALSE)), 
                 "Variable 'recursive'[:] Must have length 1")
    expect_error(exportFileRepository(rcon, 
                                      recursive = "TRUE"), 
                 "Variable 'recursive': Must be of type 'logical'")
  }
)

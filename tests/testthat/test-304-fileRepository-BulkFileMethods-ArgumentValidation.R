context("Export/import/delete FileRepository Bulk File Argument Validations")

local_file <- test_path("testdata", "FileForImportExportTesting.txt")

#####################################################################
# exportFileRepository                                           ####

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


#####################################################################
# importFileRepository

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

#####################################################################
# deleteFileRepository

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
  "Return an error when confirm is not ask, no, or yes", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = 123,
                                      confirm = "different"),
                 "'confirm'[:] Must be element of set")
  }
)

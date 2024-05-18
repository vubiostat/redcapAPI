context("export/import/delete FileRepository Single File Argument Validations")


#####################################################################
# exportFromFileRepository

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

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)

    expect_error(exportFromFileRepository(rcon, 
                                          doc_id = 123,
                                          dir = tempdir(), 
                                          dir_create = FALSE, 
                                          config = list(1)), 
                 "'config': Must have names")
    expect_error(exportFromFileRepository(rcon, 
                                          doc_id = 123,
                                          dir = tempdir(), 
                                          dir_create = FALSE, 
                                          config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportFromFileRepository(rcon, 
                                          doc_id = 123,
                                          dir = tempdir(), 
                                          dir_create = FALSE, 
                                          api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportFromFileRepository(rcon, 
                                          doc_id = 123,
                                          dir = tempdir(), 
                                          dir_create = FALSE, 
                                          api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)


#####################################################################
# impotToFileRepository

test_that(
  "Return an error if rcon is not a redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(importToFileRepository(mtcars, 
                                        file = "filename",
                                        folder_id = 123), 
                 "no applicable method for 'importToFileRepository'")
  }
)

test_that(
  "Return an error if file is not a character(1) or the file doesn't exist", 
  {
    local_reproducible_output(width = 200)
    expect_error(importToFileRepository(rcon, 
                                        file = c("file1", "file2"), 
                                        folder_id = 123), 
                 "'file'[:] Must have length 1")
    
    expect_error(importToFileRepository(rcon, 
                                        file = character(0), 
                                        folder_id = 123), 
                 "Variable 'file'[:] Must have length 1")
    
    expect_error(importToFileRepository(rcon, 
                                        file = 123, 
                                        folder_id = 123), 
                 "Variable 'file'[:] Must be of type 'character'")
    
    expect_error(importToFileRepository(rcon, 
                                        file = "this_file_does_not_exist.pdf", 
                                        folder_id = 123), 
                 "'file'[:] File does not exist[:] 'this_file_does_not_exist.pdf'")
  }
)

test_that(
  "Return an error if folder_id is not numeric(0/1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importToFileRepository(rcon, 
                                        file = "file1", 
                                        folder_id = c(12, 34)), 
                 "'folder_id'[:] Must have length [<][=] 1")
    expect_error(importToFileRepository(rcon, 
                                        file = "file1", 
                                        folder_id = "12"), 
                 "Variable 'folder_id'[:] Must be of type 'integerish'")
  }
)

#####################################################################
# deleteFromFileRepository

test_that(
  "Return an error if rcon is not a redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteFromFileRepository(mtcars, 
                                          doc_id = 123), 
                 "no applicable method for 'deleteFromFileRepository'")
  }
)

test_that(
  "Return an error if doc_id is not integerish(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteFromFileRepository(rcon, 
                                          doc_id = c(123, 234)), 
                 "'doc_id'[:] Must have length 1")
    expect_error(deleteFromFileRepository(rcon, 
                                          doc_id = "123"), 
                 "'doc_id'[:] Must be of type 'integerish'")
  }
)

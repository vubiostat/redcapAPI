context("export/import/delete FileRepository Argument Validations")

rcon <- redcapConnection(url = url, 
                         token = API_KEY)

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
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFromFileRepository(rcon, 
                                          doc_id = 123,
                                          dir = tempdir(), 
                                          dir_create = FALSE, 
                                          error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
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

test_that(
  "Return an error if refresh is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importToFileRepository(rcon, 
                                        file = "file1", 
                                        folder_id = 123, 
                                        refresh = c(TRUE, FALSE)), 
                 "'refresh'[:] Must have length 1")
    expect_error(importToFileRepository(rcon, 
                                        file = "file1", 
                                        folder_id = 123, 
                                        refresh = "TRUE"), 
                 "Variable 'refresh'[:] Must be of type 'logical'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(importToFileRepository(rcon, 
                                        file = "somefile.pdf",
                                        folder_id = 10,  
                                        error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(importToFileRepository(rcon,  
                                        file = "somefile.pdf",
                                        folder_id = 10, 
                                        config = list(1)), 
                 "'config': Must have names")
    expect_error(importToFileRepository(rcon,  
                                        file = "somefile.pdf",
                                        folder_id = 10, 
                                        config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(importToFileRepository(rcon,  
                                        file = "somefile.pdf",
                                        folder_id = 10, 
                                          api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(importToFileRepository(rcon,  
                                        file = "somefile.pdf",
                                        folder_id = 10, 
                                        api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
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

test_that(
  "Return an error if refresh is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteFromFileRepository(rcon, 
                                          doc_id = 123, 
                                          refresh = c(TRUE, FALSE)), 
                 "'refresh'[:] Must have length 1")
    expect_error(deleteFromFileRepository(rcon, 
                                          doc_id = 123, 
                                          refresh = "TRUE"), 
                 "'refresh'[:] Must be of type 'logical'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteFromFileRepository(rcon, 
                                          doc_id = 10,
                                          error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(deleteFromFileRepository(rcon,   
                                          doc_id = 10, 
                                          config = list(1)), 
                 "'config': Must have names")
    expect_error(deleteFromFileRepository(rcon,   
                                          doc_id = 10, 
                                          config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(deleteFromFileRepository(rcon,   
                                          doc_id = 10, 
                                          api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(deleteFromFileRepository(rcon,   
                                          doc_id = 10, 
                                          api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)


#####################################################################
# exportFileRepository

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

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFileRepository(rcon,
                                      folder_id = 1,
                                      error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportFileRepository(rcon, 
                                      folder_id = 1,
                                      config = list(1)), 
                 "'config': Must have names")
    expect_error(exportFileRepository(rcon, 
                                      folder_id = 1,
                                      config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportFileRepository(rcon, 
                                      folder_id = 1,
                                      api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportFileRepository(rcon, 
                                      folder_id = 1,
                                      api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
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

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(importFileRepository(rcon,
                                      dir = tempdir(),
                                      error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(importFileRepository(rcon,  
                                      dir = tempdir(), 
                                      config = list(1)), 
                 "'config': Must have names")
    expect_error(importFileRepository(rcon,  
                                      dir = tempdir(), 
                                      config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(importFileRepository(rcon,  
                                      dir = tempdir(), 
                                      api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(importFileRepository(rcon,  
                                      dir = tempdir(), 
                                      api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
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

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteFileRepository(rcon,
                                      folder_id = 123, 
                                      error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = 123,
                                      config = list(1)), 
                 "'config': Must have names")
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = 123,
                                      config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = 123,
                                      api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = 123,
                                      api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# exportFileRepositoryListing

test_that(
  "Return an error if rcon is not a redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFileRepositoryListing(mtcars), 
                 "no applicable method for 'exportFileRepositoryListing'")
  }
)

test_that(
  "Return an error if folder_id is not numeric(0/1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFileRepositoryListing(rcon, 
                                             folder_id = c(104, 105)), 
                 "'folder_id'[:] Must have length [<][=] 1")
    expect_error(exportFileRepositoryListing(rcon, 
                                             folder_id = "104"), 
                 "'folder_id'[:] Must be of type 'integerish'")
    expect_error(exportFileRepositoryListing(rcon, 
                                             folder_id = pi), 
                 "'folder_id'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error if recursive is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFileRepositoryListing(rcon, 
                                             recursive = c(TRUE, FALSE)), 
                 "'recursive'[:] Must have length 1")
    expect_error(exportFileRepositoryListing(rcon, 
                                             recursive = "TRUE"), 
                 "'recursive'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if config or api_param are not named lists", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFileRepositoryListing(rcon, 
                                             config = list("parameter")), 
                 "'config'[:] Must have names")
    expect_error(exportFileRepositoryListing(rcon, 
                                             api_param = list("parameter")), 
                 "'api_param'[:] Must have names")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFileRepositoryListing(rcon,
                                             folder_id = 123,
                                             error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportFileRepositoryListing(rcon, 
                                             folder_id = 123,
                                             config = list(1)), 
                 "'config': Must have names")
    expect_error(exportFileRepositoryListing(rcon, 
                                             folder_id = 123,
                                             config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportFileRepositoryListing(rcon, 
                                             folder_id = 123,
                                             api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportFileRepositoryListing(rcon, 
                                             folder_id = 123,
                                             api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# fileRepositoryPath

test_that(
  "Returns an error if doc_id is not integerish(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(fileRepositoryPath(doc_id = c(1, 2), 
                                    fileRepo = FileRepository), 
                 "'doc_id'[:] Must have length <= 1")
    expect_error(fileRepositoryPath(doc_id = "123", 
                                    fileRepo = FileRepository), 
                 "'doc_id'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Returns an error if folder_id is not integerish(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(fileRepositoryPath(folder_id = c(1, 2), 
                                    fileRepo = FileRepository), 
                 "'folder_id'[:] Must have length <= 1")
    expect_error(fileRepositoryPath(folder_id = "123", 
                                    fileRepo = FileRepository), 
                 "'folder_id'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Returns an error if both doc_id and folder_id have positive length", 
  {
    local_reproducible_output(width = 200)
    expect_error(fileRepositoryPath(folder_id = 1, 
                                    doc_id = 2, 
                                    fileRepo = FileRepository), 
                 "Exactly one of 'doc_id' and 'folder_id' can have length [>] 0")
  }
)


#####################################################################
# Create File Repository Folder

test_that(
  "Return an error when rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(createFileRepositoryFolder(mtcars, 
                                            name = "Folder Name"), 
                 "no applicable method for 'createFileRepositoryFolder'")
  }
)

test_that(
  "Return an error when name is not character(1) or has more than 150 characters", 
  {
    local_reproducible_output(width = 200)
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = c("Folder1", "Folder2")), 
                 "Variable 'name'[:] Must have length 1")
    
    expect_error(createFileRepositoryFolder(rcon, 
                                            123), 
                 "Variable 'name'[:] Must be of type 'character'")
    
    expect_error(createFileRepositoryFolder(rcon, 
                                            paste0(sample(letters, 
                                                          size = 151, 
                                                          replace = TRUE), 
                                                   collapse = ""), 
                                            "All elements must have at most 150 characters"))
  }
)

test_that(
  "Return an error when folder_id is not integerish(0/1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder", 
                                            folder_id = pi), 
                 "'folder_id'[:] Must be of type 'integerish'")
    
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder", 
                                            folder_id = c(2, 3)), 
                 "'folder_id'[:] Must have length [<][=] 1")
    
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder", 
                                            folder_id = "one"), 
                 "'folder_id'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error when dag_id is not integerish(0/1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder", 
                                            dag_id = pi), 
                 "'dag_id'[:] Must be of type 'integerish'")
    
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder", 
                                            dag_id = c(2, 3)), 
                 "'dag_id'[:] Must have length [<][=] 1")
    
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder", 
                                            dag_id = "one"), 
                 "'dag_id'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error when role_Id is not integerish(0/1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder", 
                                            role_id = pi), 
                 "'role_id'[:] Must be of type 'integerish'")
    
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder", 
                                            role_id = c(2, 3)), 
                 "'role_id'[:] Must have length [<][=] 1")
    
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder", 
                                            role_id = "one"), 
                 "'role_id'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error if refresh is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder", 
                                            refresh = c(TRUE, FALSE)), 
                 "'refresh': Must have length 1")
    
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder", 
                                            refresh = "TRUE"), 
                 "'refresh': Must be of type 'logical'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder",
                                            error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder",
                                            config = list(1)), 
                 "'config': Must have names")
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder",
                                            config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder",
                                            api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(createFileRepositoryFolder(rcon, 
                                            name = "folder",
                                            api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

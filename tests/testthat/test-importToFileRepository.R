context("importToFileRepository.R")

rcon <- redcapConnection(url= url, 
                         token = API_KEY)

# Only the argument validations are tested in this file. 
# See test-fileRepositoryTests.R for functional tests

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

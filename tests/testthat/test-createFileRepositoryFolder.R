context("createFileRepositoryFolder.R")

rcon <- redcapConnection(url = url, token = API_KEY)

# Only the error validations are performed in these tests. 
# with now way to delete folders, running tests that 
# create folders repeatedly is likely to make a mess of the 
# file repository

# FIXME: If deleting a folder becomes an option, Add test for creating and deleting folders



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

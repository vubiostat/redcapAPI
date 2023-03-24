context("fileRepositoryPath.R")

FileRepository <- 
  structure(list(folder_id = c(104L, 109L, 105L, 106L, NA, 107L, NA), 
                 doc_id = c(NA, NA, NA, NA, 8925315L, NA, 8937173L), 
                 name = c("Folder1", "Folder2", "Subfolder1", "Subfolder2", 
                          "laws_of_physics.png", "SubSubFolder", 
                          "laws_of_physics.png"), 
                 parent_folder = c("top-level", "top-level", "104", "104", 
                                   "104", "106", "107")), 
            row.names = c(NA, -7L), 
            class = "data.frame")

test_that(
  "Determines the correct file path", 
  {
    expect_equal(fileRepositoryPath(doc_id = 8937173, 
                                    fileRepo = FileRepository), 
                 "Folder1/Subfolder2/SubSubFolder/laws_of_physics.png")
    
    expect_equal(fileRepositoryPath(doc_id = 8925315, 
                                    fileRepo = FileRepository), 
                 "Folder1/laws_of_physics.png")
    
    expect_equal(fileRepositoryPath(fileRepo = FileRepository), 
                 "")
  }
)

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

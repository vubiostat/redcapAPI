context("Export/import/delete FileRepository Bulk File Functionality")

rcon$flush_fileRepository()
FileRepo <- rcon$fileRepository()

local_file <- test_path("testdata", "FileForImportExportTesting.txt")

# We won't test the import of a file repository because we have no way to 
# delete newly created folders. 
# We do need to put a file into the repository to test the export and delete
test_that(
  "Import a single file to the file repository", 
  {
    expect_data_frame(
      importToFileRepository(rcon, 
                             file = local_file, 
                             folder_id = FileRepo$folder_id[FileRepo$name == "SubSubFolder1A"]), 
      nrows = 1, 
      ncols = 2)
  }
)

rcon$flush_fileRepository()
FileRepo <- rcon$fileRepository()

test_that(
  "exportFileRepository with recursive = FALSE", 
  {
    test_dir <- tempdir()
    
    TestRepo <- FileRepo[!is.na(FileRepo$doc_id), ]
    
    skip_if(nrow(TestRepo) == 0, 
            "No files to export in this test. Please add a file to the repository")
    
    expect_data_frame(exportFileRepository(rcon,
                                           folder_id = as.numeric(TestRepo$parent_folder[1]),
                                           dir = test_dir,
                                           recursive = FALSE))
    
  }
)

test_that(
  "exportFileRepository with recursive = TRUE", 
  {
    test_dir <- tempdir()
    
    TestRepo <- FileRepo[!is.na(FileRepo$doc_id), ]
    
    skip_if(nrow(TestRepo) == 0, 
            "No files to export in this test. Please add a file to the repository")
    
    expect_data_frame(exportFileRepository(rcon,
                                           dir = test_dir,
                                           recursive = TRUE))
  }
)


test_that(
  "deleteFileRepository: folder has no files",
  {
    expect_message(Deleted <- deleteFileRepository(rcon, 
                                                   folder_id = 0, 
                                                   confirm = "yes"), 
                   "No files to delete")
    
    expect_data_frame(Deleted, 
                      nrows = 0, 
                      ncols = 4)
  }
)

test_that(
  "deleteFileRepository: a file exists to delete; reimport the Repository", 
  {
    td <- file.path(tempdir(), "FileRepoTesting")
    files <- list.files(td, 
                        recursive = TRUE, 
                        full.names = TRUE, 
                        all.files = TRUE, 
                        include.dirs = TRUE)
    unlink(td, recursive = TRUE, force = TRUE)
    exportFileRepository(rcon, 
                         dir = td, 
                         dir_create = TRUE,
                         recursive = TRUE)
    
    local_reproducible_output(width = 200)
   
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = 0, 
                                      recursive = TRUE, 
                                      confirm = "yes",
                                      config = list(1)), 
                 "'config': Must have names")
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = 0, 
                                      recursive = TRUE, 
                                      confirm = "yes",
                                      config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = 0, 
                                      recursive = TRUE, 
                                      confirm = "yes",
                                      api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(deleteFileRepository(rcon, 
                                      folder_id = 0, 
                                      recursive = TRUE, 
                                      confirm = "yes",
                                      api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
    
    
    Deleted <- deleteFileRepository(rcon, 
                                    folder_id = 0, 
                                    recursive = TRUE, 
                                    confirm = "yes")
    
    expect_data_frame(Deleted, 
                      ncols = 4)
  }
)

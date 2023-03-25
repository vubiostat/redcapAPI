context("export/import/deleteFileRepository Tests")

rcon <- redcapConnection(url = url, 
                         token = API_KEY)

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
                                                   folder_id = 0), 
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
    
    expect_message(Deleted <- deleteFileRepository(rcon, 
                                                   folder_id = 0, 
                                                   recursive = TRUE, 
                                                   confirm = "yes"), 
                   "File deleted[:]")
    
    expect_data_frame(Deleted, 
                      ncols = 4)
    
    # Replace the file that was deleted.
    Imported <- importFileRepository(rcon, 
                                     dir = td, 
                                     recursive = TRUE)
    
    expect_data_frame(Imported, 
                      ncols = 6)
    unlink(list.files(td, 
                      recursive = TRUE, 
                      full.names = TRUE, 
                      all.file = TRUE))
  }
)

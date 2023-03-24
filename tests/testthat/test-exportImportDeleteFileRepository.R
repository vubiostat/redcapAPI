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

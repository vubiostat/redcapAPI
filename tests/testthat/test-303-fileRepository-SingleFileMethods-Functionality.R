context("Export/Import/Delete File Repository Single File Functionality")

local_file <- test_path("testdata", "FileForImportExportTesting.txt")

#####################################################################
# Import/Export/Delete a Single File                             ####

FileRepo <- rcon$fileRepository()

test_that(
  "Import a single file to the file repository", 
  {
    expect_data_frame(
      importToFileRepository(rcon, 
                             file = local_file, 
                             folder_id = FileRepo$folder_id[FileRepo$name == "SubSubFolder"]), 
      nrows = 1, 
      ncols = 2)
  }
)

FileRepo <- rcon$fileRepository()
EXISTING_DIR <- tempdir()
DOCUMENT_ID <- FileRepo$doc_id[!is.na(FileRepo$doc_id)]
DOCUMENT_ID <- DOCUMENT_ID[1]

test_that(
  "Export a file to an existing directory", 
  {
    SavedFile <- exportFromFileRepository(rcon, 
                                          doc_id = DOCUMENT_ID, 
                                          dir = EXISTING_DIR)
    
    expect_data_frame(SavedFile, 
                      nrows = 1, 
                      ncols = 2)
  }
)

test_that(
  "File can be saved to a directory that has to be created", 
  {
    SavedFile <- exportFromFileRepository(rcon, 
                                          doc_id = DOCUMENT_ID, 
                                          dir = file.path(EXISTING_DIR, "Subfolder"), 
                                          dir_create = TRUE)
    
    expect_data_frame(SavedFile, 
                      nrows = 1, 
                      ncols = 2)
    expect_equal(basename(SavedFile$directory), 
                 "Subfolder")
  }
)

test_that(
  "File can be deleted from the File Repository", 
  {
    DeletedFile <- deleteFromFileRepository(rcon, 
                                            doc_id = DOCUMENT_ID)
    expect_data_frame(DeletedFile, 
                      nrows = 1, 
                      ncols = 2)
  }
)
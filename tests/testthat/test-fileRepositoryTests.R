context("fileRepositoryTests")

rcon <- redcapConnection(url= url, 
                         token = API_KEY)

FileRepo <- rcon$fileRepository()

DOCUMENT_ID <- FileRepo$doc_id[FileRepo$parent_folder == 114]
DOCUMENT_ID <- DOCUMENT_ID[!is.na(DOCUMENT_ID)]
DOCUMENT_ID <- head(DOCUMENT_ID, 1)

SaveFileForImport <- exportFromFileRepository(rcon, 
                                              doc_id = DOCUMENT_ID, 
                                              dir = tempdir())

# Export a file from the file repository ----------------------------

test_that(
  "File can be saved to an existing directory", 
  {
    skip_if(length(DOCUMENT_ID) == 0, 
            "No document to export. Please seed the File Repository With a Document in Folder ID 114")
    
    EXISTING_DIR <- tempdir()
    
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
    skip_if(length(DOCUMENT_ID) == 0, 
            "No document to export. Please seed the File Repository With a Document in Folder ID 114")
    
    EXISTING_DIR <- tempdir()
    
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


# Delete a file from the file repository ----------------------------

test_that(
  "File can be deleted from the File Repository", 
  {
    skip_if(length(DOCUMENT_ID), 
            "No file to delete. Please seed the File Repository With a Document in Folder ID 114")
    
    DeletedFile <- deleteFromFileRepository(rcon, 
                                            doc_id = DOCUMENT_ID)
    expect_data_frame(DeletedFile, 
                      nrows = 1, 
                      ncols = 2)
  }
)

# Import a file to the file repository ------------------------------

test_that(
  "File can be imported to the File Repository", 
  {
    skip_if(length(DOCUMENT_ID), 
            "No file to delete. Please seed the File Repository With a Document in Folder ID 114")
    
    ImportedFile <- importToFileRepository(rcon, 
                                           file = file.path(SaveFileForImport$directory, 
                                                            SaveFileForImport$filename), 
                                           folder_id = 114)
    
    expect_data_frame(ImportedFile, 
                      nrows = 1, 
                      ncols = 2)
  }
)


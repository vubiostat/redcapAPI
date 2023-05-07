context("Export/Import/Delete File Repository Functionality")

#####################################################################
# Establish a project for testing.                               ####
# It will need one record in order to have a place to store a record

purgeProject(rcon, 
             purge_all = TRUE)

load(test_path("testdata", "RedcapProject_BasicData.Rdata"))
rcon$flush_all()
restoreProject(RedcapProject_BasicData, 
               rcon)
importRecords(rcon, 
              data.frame(record_id = "1"))


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


#####################################################################
# Import/Export/Delete Multiple Files

rcon$flush_fileRepository()
FileRepo <- rcon$fileRepository()

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
    
    expect_message(Deleted <- deleteFileRepository(rcon, 
                                                   folder_id = 0, 
                                                   recursive = TRUE, 
                                                   confirm = "yes"), 
                   "File deleted[:]")
    
    expect_data_frame(Deleted, 
                      ncols = 4)
  }
)




#####################################################################
# exportFileRepositoryListing

test_that(
  "Returns a data frame", 
  {
    expect_data_frame(exportFileRepositoryListing(rcon), 
                      ncols = 4)
    expect_data_frame(exportFileRepositoryListing(rcon, 
                                                  recursive = TRUE), 
                      ncols = 4)
  }
)

#####################################################################
# fileRepositoryPath

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

#####################################################################
# createFileRepositoryFolder

# No tests performed: The API doesn't offer a way to delete folders.
# to create a new folder in testing would just make a lot of folders.
# If the API ever offers a way to delete folders in the future, 
# we can implement tests here.
#' @describeIn fileRepositoryMethods Import multiple files to the File Repository.
#' @order 2
#' @export

importFileRepository <- function(rcon, 
                                 dir, 
                                 folder_id = numeric(0), 
                                 ...){
  UseMethod("importFileRepository")
}

#' @rdname fileRepositoryMethods
#' @order 5
#' @export

importFileRepository.redcapApiConnection <- function(rcon, 
                                                     dir, 
                                                     folder_id = numeric(0),
                                                     dag_id = numeric(0), 
                                                     role_id = numeric(0), 
                                                     recursive = FALSE,
                                                     ...)
{
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_character(x = dir, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_integerish(x = folder_id, 
                               max.len = 1, 
                               any.missing = FALSE,
                               add = coll)
  
  checkmate::assert_integerish(x = dag_id, 
                               max.len = 1, 
                               add = coll)
  
  checkmate::assert_integerish(x = role_id, 
                               max.len = 1, 
                               add = coll)
  
  checkmate::assert_logical(x = recursive, 
                            len = 1, 
                            add = coll)

  checkmate::reportAssertions(coll)
  
  checkmate::assert_directory_exists(x = dir, 
                                     add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Import the Repository -------------------------------------------
  
  LocalRepo <- .importFileRepository_makeRepoStructure(dir, 
                                                       recursive = recursive)
  
  LocalRepo <- .importFileRepository_pushRepo(rcon, 
                                              LocalRepo = LocalRepo, 
                                              dir = dir,
                                              folder_id = folder_id, 
                                              dag_id = dag_id, 
                                              role_id = dag_id,
                                              ...)
  
  rcon$flush_fileRepository()
  
  LocalRepo
}

# Unexported --------------------------------------------------------

.importFileRepository_makeRepoStructure <- function(dir, recursive){
  # Directory List --------------------------------------------------
  # Make a data frame listing the directories in the Local Repository
  # We want to be able to associate each directory with its parent. 
  # This data frame will have the following columns:
  #    name - the name of the directory
  #    local_folder_id - Arbitrary integer assigned for reference 
  #         we will need this to help replicate the structure in REDCap
  #    local_doc_id - A place holder for when we add the files 
  #    local_parent_folder - The local_folder_id of the folder's parent.
  #         A 0 indicates it is at the top level of this file structure.
  DirList <- data.frame(name = list.dirs(dir, 
                                         full.names = FALSE, 
                                         recursive = recursive), 
                        stringsAsFactors = FALSE)
  
  DirList$folder_id <-                    # Assign the folder ID
    seq_len(nrow(DirList)) - recursive
  DirList$doc_id <-                       # Assign the document ID 
    rep(NA_real_, nrow(DirList))
  DirList <-                              # Remove the top-level reference
    DirList[DirList$folder_id > 0, ]
  DirList$parent_folder <- 
    rep(NA_real_, nrow(DirList))
  
  # Associate with the parent folder
  for (i in seq_len(nrow(DirList))){
    this_dir <- dirname(DirList$name[i])
    
    if (this_dir == "."){ # top-level
      DirList$parent_folder[i] <- 0
    } else {
      w <- which(DirList$name %in% this_dir)
      DirList$parent_folder[i] <- DirList$folder_id[w]
    }
  }
  
  # File List -------------------------------------------------------
  # This has the same columns as DirList. 
  # local_doc_id is an arbitrary integer, but its presence is the key
  #    to discriminating between folders and files.
  # The local_folder_id is NA for these records. 
  # The local_parent_folder maps to the local_folder_id in which the
  #    file resides
  
  # Initialize the data frame
  FileList <- data.frame(dirname = list.files(dir, 
                                              full.names = FALSE, 
                                              recursive = recursive, 
                                              include.dirs = FALSE))
  FileList$name <-               # Get the file name
    basename(FileList$dirname)
  
  FileList$dirname <-            # Get the directory the file is in 
    dirname(FileList$dirname)
  
  FileList$doc_id <-             # Assign a document ID
    seq_len(nrow(FileList))
  
  # Removing top level directories.
  # At the top level, dirname([file]) returns "." and
  #                  basename([file]) returns either the filename or directory name
  # If the basename matches the name of a folder in DirList, then we can
  #   determine that it is a folder and should not be included in the FileList
  FileList <-                    
    FileList[!(FileList$dirname == "." & 
                 FileList$name %in% DirList$name), ]
  
  # Match up with the parent folder
  FileList <-
    merge(FileList, 
          DirList[c("name", "folder_id")],
          by.x = "dirname",
          by.y = "name", 
          all.x = TRUE)
  FileList$parent_folder <-        # rename folder_id to parent_id
    FileList$folder_id
  
  FileList$folder_id <-            # populate folder_id with NAs
    rep(NA, nrow(FileList))
  
  cols <- c("name", "folder_id", "doc_id", "parent_folder")
  
  Out <- rbind(DirList[cols], 
               FileList[cols])
  
  Out$remote_folder_id <- rep(NA_real_, nrow(Out))
  Out$remote_doc_id <- rep(NA_real_, nrow(Out))
  Out$name <- basename(Out$name)
  
  Out[order(Out$parent_folder, 
            Out$folder_id, 
            Out$doc_id), ]
}


.importFileRepository_pushRepo <- function(rcon, 
                                           LocalRepo,
                                           dir, 
                                           folder_id, 
                                           dag_id, 
                                           role_id,
                                           ...){
  for (i in seq_len(nrow(LocalRepo))){
    this_folder_id <- 
      if (LocalRepo$parent_folder[i] == 0){
        folder_id
      } else {
        LocalRepo$remote_folder_id[LocalRepo$folder_id %in% 
                                     LocalRepo$parent_folder[i]]
      }
    
    if (!is.na(LocalRepo$folder_id[i])){
      NewFolder <- createFileRepositoryFolder(rcon = rcon,
                                              name = LocalRepo$name[i], 
                                              folder_id = this_folder_id, 
                                              dag_id = dag_id, 
                                              role_id = role_id,
                                              ...)
      
      LocalRepo$remote_folder_id[i] <- NewFolder$folder_id
    } else {
      this_file <- fileRepositoryPath(doc_id = LocalRepo$doc_id[i], 
                                      fileRepo = LocalRepo)
      this_file <- file.path(dir, this_file)
      
      LocalRepo$remote_folder_id[i] <- 
        LocalRepo$remote_folder_id[LocalRepo$folder_id %in% LocalRepo$parent_folder[i]]

      NewFile <- importToFileRepository(rcon, 
                                        file = this_file, 
                                        folder_id = this_folder_id,
                                        ...)
    }
  }
  
  LocalRepo
}

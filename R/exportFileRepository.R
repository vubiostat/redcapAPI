#' @describeIn fileRepositoryMethods Export multiple files from the File Repository.
#' @order 1
#' @export

exportFileRepository <- function(rcon,
                                 folder_id,
                                 dir = getwd(),
                                 dir_create = FALSE,
                                 recursive = FALSE,
                                 ...){
  UseMethod("exportFileRepository")
}

#' @rdname fileRepositoryMethods
#' @order 4
#' @export

exportFileRepository.redcapApiConnection <- function(rcon,
                                                     folder_id = numeric(0),
                                                     dir = getwd(),
                                                     dir_create = FALSE,
                                                     recursive = FALSE,
                                                     ...)
{
  # Argument Validation ---------------------------------------------

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)

  checkmate::assert_integerish(x = folder_id,
                               max.len = 1,
                               any.missing = FALSE,
                               add = coll)

  checkmate::assert_character(x = dir,
                              len = 1,
                              add = coll)

  checkmate::assert_logical(x = dir_create,
                            len = 1,
                            add = coll)

  checkmate::assert_logical(x = recursive,
                            len = 1,
                            add = coll)

  checkmate::reportAssertions(coll)

  if (!dir_create){
    checkmate::assert_directory_exists(x = dir,
                                       add = coll)
    checkmate::reportAssertions(coll)
  }

  # Create the directory if it does not exist ------------------------

  if (dir_create && !file.exists(dir)) {
    dir.create(dir,
               recursive = TRUE)
  }

  # Fetch the files to download -------------------------------------

  FileRepo <- exportFileRepositoryListing(rcon,
                                          folder_id = folder_id,
                                          recursive = recursive)

  if (nrow(FileRepo) == 0){
    logMessage("No files or folders to download")
    return(FILE_IMPORT_EXPORT_EMPTY_FRAME)
  }

  # Export the File Repository --------------------------------------

  ExportedFiles <- vector("list", nrow(FileRepo))
  n_folder_created <- 0

  for (i in seq_len(nrow(FileRepo))){
    # Export a file
    if (!is.na(FileRepo$doc_id)[i]){
      this_doc <- FileRepo$doc_id[i]
      filepath <- fileRepositoryPath(doc_id = this_doc,
                                     fileRepo = FileRepo)
      this_dir <- dirname(filepath)

      .exportFileRepository_createLocalDirectory(dir, this_dir)

      ExportedFiles[[i]] <- exportFromFileRepository(rcon,
                                                     doc_id = this_doc,
                                                     dir = file.path(dir, this_dir),
                                                     dir_create = TRUE,
                                                     ...)
    } else {
      this_folder <- FileRepo$folder_id[i]
      this_dir <- fileRepositoryPath(folder_id = this_folder,
                                     fileRepo = FileRepo)

      .exportFileRepository_createLocalDirectory(dir, this_dir)

      n_folder_created <- n_folder_created + 1
      ExportedFiles[[i]] <- data.frame(directory = file.path(dir, this_dir),
                                       filename = NA_character_,
                                       stringsAsFactors = FALSE)
    } # end if/else
  } # end for

  do.call("rbind", ExportedFiles)
}


# Unexported --------------------------------------------------------

.exportFileRepository_createLocalDirectory <- function(dir, subdir){
  dir_to_create <- file.path(dir, subdir)
  if (!file.exists(dir_to_create)){
    dir.create(dir_to_create,
               recursive = TRUE)
  }
}

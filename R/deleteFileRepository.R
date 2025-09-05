#' @describeIn fileRepositoryMethods Delete multiple files from the File Repository.
#' @order 3
#' @export

deleteFileRepository <- function(rcon,
                                 folder_id,
                                 recursive = FALSE,
                                 ...){
  UseMethod("deleteFileRepository")
}

#' @rdname fileRepositoryMethods
#' @order 6
#' @export

deleteFileRepository.redcapApiConnection <- function(
  rcon,
  folder_id,
  recursive      = FALSE,
  ...,
  confirm        = c("ask", "no", "yes"))
{
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)

  checkmate::assert_integerish(x = folder_id,
                               len = 1,
                               any.missing = FALSE,
                               add = coll)

  checkmate::assert_logical(x = recursive,
                            len = 1,
                            add = coll)

  confirm <- checkmate::matchArg(x = confirm,
                                 choices = c("ask", "no", "yes"),
                                 add = coll,
                                 .var.name = "confirm")

  checkmate::reportAssertions(coll)

  # Determine how many files will be deleted ------------------------

  if (folder_id == 0) folder_id <- numeric(0)

  ToDelete <- exportFileRepositoryListing(rcon,
                                          folder_id = folder_id,
                                          recursive = recursive)

  files_to_delete <- sum(!is.na(ToDelete$doc_id))

  if (files_to_delete == 0){
    logMessage("No files to delete in the requested folder(s)")
    return(FILE_REPOSITORY_EMPTY_FRAME)
  }

  # Get confirmation ------------------------------------------------

  if (confirm == "ask"){
    confirm <-
      readline(prompt = "Type 'yes' to confirm the delete action.   ")

    confirm = trimws(confirm)

    if (tolower(confirm) != "yes") confirm <- "no"
  }

  if (confirm == "no"){
    logMessage("Delete action cancelled by user.")
    return(FILE_REPOSITORY_EMPTY_FRAME)
  }

  # Delete the files ------------------------------------------------

  ToDelete <- ToDelete[!is.na(ToDelete$doc_id), ]

  for (i in seq_len(nrow(ToDelete))){
    deleteFromFileRepository(rcon,
                             doc_id = ToDelete$doc_id[i], ...)
  }

  rcon$flush_fileRepository()

  ToDelete
}

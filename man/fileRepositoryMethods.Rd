% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docsFileRepositoryMethods.R,
%   R/exportFileRepository.R, R/importFileRepository.R, R/deleteFileRepository.R
\name{fileRepositoryMethods}
\alias{fileRepositoryMethods}
\alias{exportFileRepository}
\alias{importFileRepository}
\alias{deleteFileRepository}
\alias{exportFileRepository.redcapApiConnection}
\alias{importFileRepository.redcapApiConnection}
\alias{deleteFileRepository.redcapApiConnection}
\title{Export, Import, or Delete Multiple Files from the File Repository}
\usage{
exportFileRepository(
  rcon,
  folder_id,
  dir = getwd(),
  dir_create = FALSE,
  recursive = FALSE,
  ...
)

importFileRepository(rcon, dir, folder_id = numeric(0), ...)

deleteFileRepository(rcon, folder_id, recursive = FALSE, ...)

\method{exportFileRepository}{redcapApiConnection}(
  rcon,
  folder_id = numeric(0),
  dir = getwd(),
  dir_create = FALSE,
  recursive = FALSE,
  ...
)

\method{importFileRepository}{redcapApiConnection}(
  rcon,
  dir,
  folder_id = numeric(0),
  dag_id = numeric(0),
  role_id = numeric(0),
  recursive = FALSE,
  ...
)

\method{deleteFileRepository}{redcapApiConnection}(
  rcon,
  folder_id,
  recursive = FALSE,
  ...,
  confirm = c("ask", "no", "yes")
)
}
\arguments{
\item{rcon}{A \code{redcapConnection} object.}

\item{folder_id}{\code{integerish(0/1)} The folder ID with the files to
download. If length 0, defaults to the top-level directory.}

\item{dir}{\code{character(1)}. A directory on the local system to which
the files are to be saved. Defaults to the working directory.}

\item{dir_create}{\code{logical(1)}. When \code{TRUE} create the directory
\code{dir} if it does not already exist. Defaults to \code{FALSE}.
If \code{dir} does not exist and \code{dir_create = FALSE}, an error is thrown.}

\item{dag_id}{\code{integerish(0/1)} The ID of a data access group. When
provided, access to the folder will be restricted to the DAG.}

\item{role_id}{\code{integerish(0/1)} The ID of a role. When provided,
access to the folder will be restricted to users with that role.}

\item{recursive}{\code{logical(1)}. When \code{TRUE}, export all subfolders
and their files as well.}

\item{confirm}{\code{character}. One of \code{c("ask", "no", "yes")}.
When \code{"ask"}, user will be prompted to confirm the deletion.
When \code{"no"}, the function will terminate with no action. When
\code{"yes"}, the function will proceed without confirmation (useful
for automated processes).}

\item{...}{Arguments to pass to other methods}
}
\value{
\code{exportFileRepository} returns a data frame with the locations to which
the files were saved on the local system. It has the columns:\tabular{ll}{
    \tab  \cr
   \code{directory} \tab The directory in which the file is saved. \cr
   \code{filename} \tab The name of the saved file. \cr
}


\code{importFileRepository} returns a data frame with the locations to which
the files were saved on the local system. It has the columns:\tabular{ll}{
    \tab  \cr
   \code{directory} \tab The directory in which the file is saved. \cr
   \code{filename} \tab The name of the saved file. \cr
}


\code{deleteFileRepository} returns a data frame listing the files that
were deleted from the file repository. It has the columns:\tabular{ll}{
    \tab  \cr
   \code{folder_id} \tab The REDCap assigned ID number for the folder. This will be \code{NA} for files. \cr
   \code{doc_id} \tab The REDCap assigned ID number for the file. \cr
   \code{name} \tab The filename of the deleted files. \cr
   \code{parent_folder} \tab The folder ID of parent folder. \cr
}
}
\description{
These methods enable the user to export, import, or delete
entire folders of files from the file repository. These actions
may be done recursively to include subfolders as well.
}
\details{
\code{deleteFileRepository} will only delete files and cannot
delete folders.

Deleted files will remain in the recycling bin for up to 30 days.
}
\section{Functions}{
\itemize{
\item \code{exportFileRepository()}: Export multiple files from the File Repository.

\item \code{importFileRepository()}: Import multiple files to the File Repository.

\item \code{deleteFileRepository()}: Delete multiple files from the File Repository.

}}
\examples{
\dontrun{
unlockREDCap(connections = c(rcon = "project_alias"), 
             url = "your_redcap_url", 
             keyring = "API_KEYs", 
             envir = globalenv())
             
save_location <- tempdir()

# Export the top-level file repository folder
exportFileRepository(rcon, 
                     folder_id = numeric(0), 
                     dir = save_location)
                     
# Export the entire repository 
exportFileRepository(rcon, 
                     folder_id = numeric(0), 
                     dir = save_location, 
                     recursive = TRUE)
                     
# Export a file repository folder below the top-level
exportFileRepository(rcon, 
                     folder_id = 12345, 
                     dir = save_location)
                     
# Import the files from a folder to the top-level file repository
importFileRepository(rcon, 
                     dir = "path/to/folder")
                     
# Import the files from a folder to sub folder of the file repository
importFileRepository(rcon, 
                     dir = "path/to/folder", 
                     folder_id = 12345) 
                     
# Import the files from a folder and assign to a specific 
# Data Access Group
importFileRepository(rcon, 
                     dir = "path/to/folder", 
                     dag_id = 789)  
                     
# Delete files from the top-level folder of the file repository
deleteFileRepository(rcon, 
                     folder_id = numeric(0))
                     
# Delete all the file sfrom the file repository
deleteFileRepository(rcon, 
                     folder_id = numeric(0), 
                     recursive = TRUE)                   
}

}
\seealso{
\code{\link[=exportFromFileRepository]{exportFromFileRepository()}}, \cr
\code{\link[=importToFileRepository]{importToFileRepository()}}, \cr
\code{\link[=deleteFromFileRepository]{deleteFromFileRepository()}}, \cr
\code{\link[=exportFileRepositoryListing]{exportFileRepositoryListing()}}, \cr
\code{\link[=createFileRepositoryFolder]{createFileRepositoryFolder()}}
}

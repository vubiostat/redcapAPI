ThisRepo <- rcon$fileRepository()

# There must be a "Folder1" in the top level directory
which_folder1 <- which(ThisRepo$name %in% "Folder1")
is_root_folder1 <- ThisRepo$parent_folder[which_folder1] == 0
is_ok_folder1 <- length(which_folder1) == 1 && is_root_folder1

# There must be a "Folder2" in the top level directory
which_folder2 <- which(ThisRepo$name %in% "Folder2")
is_root_folder2 <- ThisRepo$parent_folder[which_folder2] == 0
is_ok_folder2 <- length(which_folder2) == 1 && is_root_folder2

# There must be a "SubFolder1A" that is a child of Folder 1
which_subfolder1A <- which(ThisRepo$name %in% "SubFolder1A")
has_ancestry_subfolder1A <- ThisRepo$parent_folder[which_subfolder1A] == ThisRepo$folder_id[which_folder1]
is_ok_subfolder1A <- length(which_subfolder1A) == 1 && has_ancestry_subfolder1A

# There must be a "SubFolder1B" that is a child of Folder 1
which_subfolder1B <- which(ThisRepo$name %in% "SubFolder1B")
has_ancestry_subfolder1B <- ThisRepo$parent_folder[which_subfolder1B] == ThisRepo$folder_id[which_folder1]
is_ok_subfolder1B <- length(which_subfolder1B) == 1 && has_ancestry_subfolder1B

# There must be a "SubSubFolder1BA" that is a child of SubFolder1A
which_subsubfolder1A <- which(ThisRepo$name %in% "SubSubFolder1A")
has_ancestry_subsubfolder1A <- ThisRepo$parent_folder[which_subsubfolder1A] == ThisRepo$folder_id[which_subfolder1A]
is_ok_subsubfolder1A <- length(which_subsubfolder1A) == 1 && has_ancestry_subsubfolder1A

# There must be a "SubFolder2A" that is a child of Folder 2
which_subfolder2A <- which(ThisRepo$name %in% "SubFolder2A")
has_ancestry_subfolder2A <- ThisRepo$parent_folder[which_subfolder2A] == ThisRepo$folder_id[which_folder2]
is_ok_subfolder2A <- length(which_subfolder2A) == 1 && has_ancestry_subfolder2A

is_ok <- 
  is_ok_folder1 && is_ok_folder2 &&
  is_ok_subfolder1A && is_ok_subfolder1B && is_ok_subsubfolder1A &&
  is_ok_subfolder2A


if (!is_ok){
  temp <- tempdir()
  rep_structure <- file.path(temp, c("Repo/Folder1", 
                                     "Repo/Folder2", 
                                     "Repo/Folder1/SubFolder1A", 
                                     "Repo/Folder1/SubFolder1B", 
                                     "Repo/Folder1/SubFolder1A/SubSubFolder1A", 
                                     "Repo/Folder2/SubFolder2A"))
  lapply(rep_structure, dir.create, recursive = TRUE)
  
  tryCatch(importFileRepository(rcon, 
                                file.path(temp, "Repo"), 
                                recursive = TRUE), 
           error = function(cond) warning("The File Repository Structure could not be created. File Repository Tests may not run as intended."))

  unlink(file.path(temp, "Repo"), recursive = TRUE)
}

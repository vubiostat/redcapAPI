  #############################################################################
 #
# To properly do a full integration test a REDCap
# instance is required that matches the intended test cases.
#
# This helper will pull the required values from a keyring
# similar to how `rccola` works for securitiy purposes.
# This package cannot depend on `rccola` without creating 
# a circular dependency so minimal code is copied locally
# 
# To duplicate our test database see: inst/extdata
#
# Create a keyring with
# 
#   options(keyring_backend=keyring::backend_file) # Because MACOS is so irritating
#   keyring::keyring_create('TestRedcapAPI', 'YOURPASSWORDHERE')
#   keyring::key_set_with_value('TestRedcapAPI', username='TestRedcapAPI', keyring='TestRedcapAPI', password='YOURAPIKEYHERE')
# To remove invalid password/API_KEY
#   keyring::key_delete('TestRedcapAPI', 'TestRedcapAPI', 'TestRedcapAPI')

url <- "https://redcap.vanderbilt.edu/api/" # Our institutions REDCap instance
if(!exists("password")){
  password <- getPass::getPass("Enter Password for keyring 'testRedcapAPI'")
}

if(!exists("API_KEY")){
  keyring::keyring_unlock('TestRedcapAPI', password)
  API_KEY <- keyring::key_get('TestRedcapAPI', 'TestRedcapAPI', 'TestRedcapAPI')
}

library(checkmate) # for additional expect_* functions.



# Helper copied from Hadley Wickham's helpr package
# Used for testing mChoice
installed_packages <- function() {
  user_libPaths <- normalizePath(.libPaths())
  uniqueLibPaths <- subset(user_libPaths, !duplicated(user_libPaths))
  
  paths <- unlist(lapply(uniqueLibPaths, dir, full.names = TRUE))
  desc <- file.path(paths, "DESCRIPTION")
  desc <- desc[file.exists(desc)]
  
  dcf <- lapply(desc, read.dcf, fields = c("Package", "Title", "Version"))
  packages <- as.data.frame(do.call("rbind", dcf), stringsAsFactors = FALSE)
  
  packages$status <- ifelse(packages$Package %in% .packages(), "loaded", "installed")
  class(packages) <- c("packages", class(packages))
  packages <- packages[order(packages$Package), ]
  packages[! duplicated(packages$Package), ]
  packages$Package
}
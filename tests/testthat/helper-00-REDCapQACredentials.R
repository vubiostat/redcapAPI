  #############################################################################
 #
# To properly do a full integration test a REDCap
# instance is required that matches the intended test cases.
#
# This helper will pull the required values from a keyring
# similar to how `rccola` works for security purposes.
# This package cannot depend on `rccola` without creating 
# a circular dependency so minimal code is copied locally
# 
# To duplicate our test database see: inst/extdata
#
# Create a keyring: 
# This will create a keyring "API_KEYs"
# It will name it the service "redcapAPI"
# It will ask to save an API_KEY in this ring (there can be multiple!)
#   of the name "TestRedcapAPI"
  
library(checkmate) # for additional expect_* functions.
library(keyring)
  
url <- "https://redcap.vanderbilt.edu/api/" # Our institutions REDCap instance

unlockREDCap(
  c(rcon_orig ="TestRedcapAPI",
    rcon = "DataTypes"),
  # c(rcon = "TestRedcapAPI"), 
  url=url, keyring='API_KEYs', 
  envir=globalenv())

library(checkmate) # for additional expect_* functions.

EXPENDABLE_USER <- "bstat_api_user"
EXPORT_REPORTS_ID <- 375181

TEST_START_TIME <- Sys.time()
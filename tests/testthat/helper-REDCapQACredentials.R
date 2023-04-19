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
# This will create a keyring "API_KEYs"
# It will name it the service "redcapAPI"
# It will ask to save an API_KEY in this ring (there can be multiple!)
#   of the name "TestRedcapAPI"
url <- "https://redcap.vanderbilt.edu/api/" # Our institutions REDCap instance

unlockREDCap(c(conn="TestRedcapAPI"), url=url, keyring='API_KEYs')

if(!exists("API_KEY"))
  API_KEY <- keyring::key_get('redcapAPI', 'TestRedcapAPI', 'API_KEYs')


STRUCTURAL_TEST_READY <- FALSE

library(checkmate) # for additional expect_* functions.

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

conns <- unlockREDCap(
  c(rcon ="TestRedcapAPI"), # Your default from keyring
  #c(rcon = "YourChoiceOfKeyHere"),
  url=url, keyring='API_KEYs', 
  envir=globalenv())

  ############################################################################
 #
#  Uncomment to create all API Keys with names in keylocker,
#  with TestRedcapAPI being ones default. This allows one to change the above
#  rcon easily for desired target. For convenience, the REPORT_IDS
#  for each environment is listed as well
#
# unlockREDCap(
#   c(rcon = "TestRedcapAPI", # Desired default, Get REPORT_ID from below list
#     a1   = "SandboxTest",   # pid 167416, Sys.setenv(REPORT_IDS=410354)
#     a2   = "QATest",        # pid 167509, Sys.setenv(REPORT_IDS='357209,362756')
#     a3   = "DevTest",       # pid 167805, Sys.setenv(REPORT_IDS='362274,375181')
#     a4   = "ExprTest",      # pid 174218, Sys.setenv(REPORT_IDS='371898,371899')
#     a5   = "ThomasTest",    # pid 178186, Sys.setenv(REPORT_IDS='384516,384517')
#     a6   = "ShawnTest",     # pid 188425, Sys.setenv(REPORT_IDS=417554)
#     a7   = "DQTest"         # pid 133406, Sys.setenv(REPORT_IDS=NULL)
#     ), 
#   url=url, keyring='API_KEYs')

missing_codes <- conns$rcon$projectInformation()$missing_data_codes

if(!is.na(missing_codes) && nchar(missing_codes) > 0)
  stop("The test suite will fail if missing data codes are defined in the project.")

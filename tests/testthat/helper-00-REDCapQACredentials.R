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
# Create a shelter key store:
# This will create a secret keyring "API_KEYs"
# It will ask to save an API_KEY in this ring (there can be multiple!)
#   of the name "TestRedcapAPI"

library(checkmate) # for additional expect_* functions.

# usethis::edit_r_profile() # <== This will always find .Rprofile

# Override using the environment variables:
#
# * REDCAP_URL: Your institutions URL
# * REDCAP_TESTDB_NAME: The name in your keyring for the test project instance
# * REDCAP_DQDBNAME: The name in your keyring for the data quality project instance
# * REDCAP_KEYRING: The name of your keyring
#
url     <- Sys.getenv("REDCAP_URL",         "https://redcap.vumc.org/api/")
testdb  <- Sys.getenv("REDCAP_TESTDB_NAME", "TestRedcapAPI") # reference in keyring
dqdb    <- Sys.getenv("REDCAP_DQDB_NAME",   "") # "DQTest") Data Quality REDCap project
keyring <- Sys.getenv("REDCAP_KEYRING",     "API_KEYs")

RUN_DATAQUALITY_TEST <- dqdb != ''
databases <- if(RUN_DATAQUALITY_TEST)
               c(rcon = testdb, dqrcon = dqdb) else
               c(rcon = testdb)

unlockREDCap(
  databases, # Open the keyring name as the variable rcon
  url     = url,    # Using the url
  keyring = keyring,# from the defined keyring
  envir   = environment())      # in the global environment

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

missing_codes <- rcon$projectInformation()$missing_data_codes

if(!is.na(missing_codes) && nchar(missing_codes) > 0)
  stop("The test suite will fail if missing data codes are defined in the project.")

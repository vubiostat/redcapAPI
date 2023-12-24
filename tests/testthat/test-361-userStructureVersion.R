context("REDCap user structure versioning")

User <- exportUsers(rcon)

test_that("User structure contains api_modules starting at 14.0.3",
{
  expect_false("api_modules" %in% names(redcapUserStructure("1.0.3")))
  expect_false("api_modules" %in% names(redcapUserStructure("14.0.2")))
  expect_true("api_modules" %in% names(redcapUserStructure("14.0.3")))
})

test_that("Prep user import pre 14.0.3 fails if given api_modules",
{
  local_reproducible_output(width = 200)
  old_rcon <- rcon
  old_rcon[['version']] <- function(){ "14.0.2" }
  
  expect_error(prepUserImportData(User, 
                                  rcon = old_rcon), 
               "api_modules")
  
  old_rcon[['version']] <- function(){ "14.0.3" }
  expect_no_error(prepUserImportData(User, rcon = old_rcon))
})
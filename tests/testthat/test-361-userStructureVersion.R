User <- exportUsers(rcon)

UserStructV_1_0_3 <- names(redcapUserStructure("1.0.3"))
UserStructV_14_0_2 <- names(redcapUserStructure("14.0.2"))
UserStructV_14_0_3 <- names(redcapUserStructure("14.0.3"))
UserStructV_14_3_4 <- names(redcapUserStructure("14.3.4"))
UserStructV_14_4_0 <- names(redcapUserStructure("14.4.0"))
UserStructV_16_0_0 <- names(redcapUserStructure("16.0.0"))
NewestStruct <- UserStructV_16_0_0
# determine newer columns
PostV_14_0_3 <- setdiff(NewestStruct, UserStructV_14_0_3)
PostV_14_4_0 <- setdiff(NewestStruct, UserStructV_14_4_0)
# remove newer columns
UserV_14_0_3 <- User[, setdiff(names(User), PostV_14_0_3)]
UserV_14_4_0 <- User[, setdiff(names(User), PostV_14_4_0)]

test_that("User structure contains api_modules starting at 14.0.3",
{
  expect_false("api_modules" %in% UserStructV_1_0_3)
  expect_false("api_modules" %in% UserStructV_14_0_2)
  expect_true("api_modules" %in% UserStructV_14_0_3)
})

test_that("User structure contains email_logging starting at 14.4.0",
{
  expect_false("email_logging" %in% UserStructV_1_0_3)
  expect_false("email_logging" %in% UserStructV_14_3_4)
  expect_true("email_logging" %in% UserStructV_14_4_0)
})

test_that("User structure contains data_access_group_label starting at 16.0.0",
{
  expect_false("data_access_group_label" %in% UserStructV_1_0_3)
  expect_false("data_access_group_label" %in% UserStructV_14_4_0)
  expect_true("data_access_group_label" %in% UserStructV_16_0_0)
})

test_that("Prep user import pre 14.4.0 fails if given email_logging",
{
  local_reproducible_output(width = 200)
  old_rcon <- rcon
  old_rcon[['version']] <- function(){ "14.3.4" }

  expect_error(prepUserImportData(User,
                                  rcon = old_rcon),
               "email_logging")

  old_rcon[['version']] <- function(){ "14.4.0" }
  expect_no_error(prepUserImportData(UserV_14_4_0, rcon = old_rcon))
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
  expect_no_error(prepUserImportData(UserV_14_0_3, rcon = old_rcon))
})

test_that("Prep user import pre 16.0.0 fails if given data_access_group_label",
{
  local_reproducible_output(width = 200)
  old_rcon <- rcon
  old_rcon[['version']] <- function(){ "14.4.0" }

  expect_error(prepUserImportData(User,
                                  rcon = old_rcon),
               "data_access_group_label")

  old_rcon[['version']] <- function(){ "16.0.0" }
  expect_no_error(prepUserImportData(User, rcon = old_rcon))
})

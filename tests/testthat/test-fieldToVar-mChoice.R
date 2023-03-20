context("fieldToVar mChoice")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that("records can be exported with Hmisc attached",{
  skip_if(!requireNamespace("Hmisc", quietly=TRUE), 
          "Hmisc is required to test mChoice export")
  expect_silent(rec <- exportRecords(rcon))
  expect_gte(length(rec), 1)
})

test_that("mChoice type conversion for checkbox with Hmisc attached",{
  skip_if(!requireNamespace("Hmisc", quietly=TRUE), 
          "Hmisc is required to test mChoice type conversion")
  rec <- exportRecords(rcon)
  expect_class(rec$prereq_checkbox, "mChoice")
  
  rec <- exportRecords(rcon, mChoice=TRUE)
  expect_class(rec$prereq_checkbox, "mChoice")
  
  rec <- exportRecords(rcon, mChoice=FALSE)
  expect_false("prereq_checkbox" %in% names(rec))
})
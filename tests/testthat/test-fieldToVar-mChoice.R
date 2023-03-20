context("fieldToVar mChoice")

require(Hmisc)

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

test_that("mChoice respects labels",{
  rec <- exportRecords(rcon, labels=TRUE)
  expect_equal(levels(rec$prereq_checkbox), c("Checkbox1", "Checkbox2", "CheckboxABC", "Do not use in branching logic"))

  rec <- exportRecords(rcon, labels=FALSE)
  
  expect_equal(levels(rec$prereq_checkbox), c("1", "2", "ABC", "4"))
               
})
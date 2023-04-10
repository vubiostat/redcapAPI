context("exportRecordsTyped mChoice")

require(Hmisc)

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "records can be exported with Hmisc attached",
  {
    skip_if(!requireNamespace("Hmisc", quietly=TRUE), 
            "Hmisc is required to test mChoice export")
    rec <- exportRecordsTyped(rcon)
    expect_gte(length(rec), 1)
  }
)

test_that(
  "mChoice type conversion for checkbox with Hmisc attached",
  {
    skip_if(!requireNamespace("Hmisc", quietly=TRUE), 
            "Hmisc is required to test mChoice type conversion")
    
    rec <- exportRecordsTyped(rcon)
    expect_class(rec$prereq_checkbox, "mChoice")
    
    rec <- exportRecordsTyped(rcon, mChoice=TRUE)
    expect_class(rec$prereq_checkbox, "mChoice")
    
    rec <- exportRecordsTyped(rcon, mChoice=FALSE)
    expect_false("prereq_checkbox" %in% names(rec))
    
    rec <- exportRecordsTyped(rcon, mChoice="labelled")
    expect_class(rec$prereq_checkbox, "mChoice")
    
    rec <- exportRecordsTyped(rcon, mChoice="coded")
    expect_class(rec$prereq_checkbox, "mChoice")
    
  }
)

test_that(
  "mChoice works for coded",
  {
    rec <- exportRecordsTyped(rcon, mChoice="coded")
    expect_equal(levels(rec$prereq_checkbox), c("1", "2", "ABC", "4"))
  }
)

test_that(
  "mChoice works for TRUE and defaults to coded",
  {
    rec <- exportRecordsTyped(rcon, mChoice=TRUE)
    expect_equal(levels(rec$prereq_checkbox), c("1", "2", "ABC", "4"))
  }
)

test_that(
  "mChoice works for NULL and defaults to coded",
  {
    rec <- exportRecordsTyped(rcon)
    expect_equal(levels(rec$prereq_checkbox), c("1", "2", "ABC", "4"))
  }
)


test_that(
  "mChoice works for labelled",
  {
    rec <- exportRecordsTyped(rcon, mChoice="labelled")
    expect_equal(levels(rec$prereq_checkbox),
                 c("Checkbox1", "Checkbox2", "CheckboxABC", "Do not use in branching logic"))
  }
)

# Without Hmisc Tests
detach("package:Hmisc", unload=TRUE)

test_that(
  "mChoice with no Hmisc warns user if requested",
  {
    expect_warning(rec <- exportRecordsTyped(rcon, mChoice=TRUE), "Hmisc")
    expect_false("prereq_checkbox" %in% names(rec))
    
    expect_warning(rec <- exportRecordsTyped(rcon, mChoice="coded"), "Hmisc")
    expect_false("prereq_checkbox" %in% names(rec))
    
    expect_warning(rec <- exportRecordsTyped(rcon, mChoice="labelled"), "Hmisc")
    expect_false("prereq_checkbox" %in% names(rec))
  }
)
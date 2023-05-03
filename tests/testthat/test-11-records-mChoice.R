
 ####################################################################
# mChoice specific tests

require(Hmisc)
 

test_that(
  "mChoice type conversion for checkbox with Hmisc attached",
  {
    skip_if(!requireNamespace("Hmisc", quietly=TRUE), 
            "Hmisc is required to test mChoice type conversion")
    
    rec <- exportRecordsTyped(rcon) |> mChoiceCast(rcon, style="labelled") 
    expect_class(rec$prereq_checkbox, "mChoice")
    
    rec <- exportRecordsTyped(rcon) |> mChoiceCast(rcon, style="coded")
    expect_class(rec$prereq_checkbox, "mChoice")
    
  }
)

test_that(
  "mChoice works for coded",
  {
    rec <- exportRecordsTyped(rcon) |> mChoiceCast(rcon, style="coded")
    expect_equal(levels(rec$prereq_checkbox), c("1", "2", "ABC", "4"))
  }
)


test_that(
  "mChoice defaults to labelled",
  {
    rec <- exportRecordsTyped(rcon) |> mChoiceCast(rcon)
    expect_equal(levels(rec$prereq_checkbox), c("Checkbox1", "Checkbox2", "CheckboxABC", "Do not use in branching logic"))
  }
)


test_that(
  "mChoice works for labelled",
  {
    rec <- exportRecordsTyped(rcon) |> mChoiceCast(rcon, style="labelled")
    expect_equal(levels(rec$prereq_checkbox),
                 c("Checkbox1", "Checkbox2", "CheckboxABC", "Do not use in branching logic"))
  }
)

# Without Hmisc Tests
detach("package:Hmisc", unload=TRUE)

test_that(
  "mChoice with no Hmisc warns user if requested",
  {
    expect_warning(rec <- exportRecordsTyped(rcon) |> mChoiceCast(rcon), "Hmisc") 

    expect_warning(rec <- exportRecordsTyped(rcon) |> mChoiceCast(rcon, "coded"), "Hmisc")
   
    expect_warning(rec <- exportRecordsTyped(rcon) |> mChoiceCast(rcon, "labelled"), "Hmisc")
  }
)

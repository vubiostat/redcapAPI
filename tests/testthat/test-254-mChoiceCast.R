context("mChoiceCast.R")


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
  "mChoice does not drop additional attributes",
  {
    rec <- exportRecordsTyped(rcon)
    attr(rec, "something") <- "wicked"
    rec <- mChoiceCast(rec, rcon, style="coded")
    
    expect_equal(attr(rec, "something"), "wicked")
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

# drop_fields tests
columns_to_check <- c("prereq_checkbox___1","prereq_checkbox___2","prereq_checkbox___abc","prereq_checkbox___4","no_prereq_checkbox___1","no_prereq_checkbox___2","no_prereq_checkbox___3",
                      "checkbox_test___x","checkbox_test___y","checkbox_test___z")

# default drop_fields is TRUE
test_that(
  "mChoice drop_fields defaults to TRUE works to drop suffixed checkbox fields",
  {
    rec <- exportRecordsTyped(rcon) |> mChoiceCast(rcon, style="labelled")
    expect_false(any(columns_to_check %in% colnames(rec)), "Expected column names are present in the data frame.")
  }
)

# drop_fields=TRUE
test_that(
  "mChoice drop_fields defaults to TRUE works to drop suffixed checkbox fields",
  {
    rec <- exportRecordsTyped(rcon) |> mChoiceCast(rcon, style="labelled", drop_fields=TRUE)
    expect_false(any(columns_to_check %in% colnames(rec)), "Expected column names are present in the data frame.")
  }
)

# drop_fields = FALSE
test_that(
  "mChoice drop_fields defaults to TRUE works to drop suffixed checkbox fields",
  {
    rec <- exportRecordsTyped(rcon) |> mChoiceCast(rcon, style="labelled", drop_fields=FALSE)
    expect_true(any(columns_to_check %in% colnames(rec)), "Expected column names are not present in the data frame.")
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


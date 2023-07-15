context("fieldChoiceMapping Functionality")

test_that(
  "REGEX_MULT_CHOICE_STRICT doesn't allow legacy short specification",
  {
    expect_false(grepl(REGEX_MULT_CHOICE_STRICT, "x | y | z"))
    expect_true(grepl(REGEX_MULT_CHOICE_STRICT, "1, x | 2, y | 3, z"))
    expect_false(grepl(REGEX_MULT_CHOICE_STRICT, "x | 2, y | z"))
  }
)

test_that(
  "REGEX_MULT_CHOICE accepts legacy and current but not mixed",
  {
    expect_true(grepl(REGEX_MULT_CHOICE, "x | y | z"))
    expect_true(grepl(REGEX_MULT_CHOICE, "1, x | 2, y | 3, z"))
    expect_false(grepl(REGEX_MULT_CHOICE, "x | 2, y | z"))  
  }
)

test_that(
  "Return an error if object is neither character nor redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(fieldChoiceMapping(mtcars), 
                 "no applicable method for 'fieldChoiceMapping'")
  }
)

test_that(
  "Return an error if the character string has length > 1", 
  {
    local_reproducible_output(width = 200)
    expect_error(fieldChoiceMapping(c("1, Red | 2, Blue", 
                                      "a, Lasagna | b, Spaghetti")), 
                 "'object'[:] Must have length 1")
    expect_error(fieldChoiceMapping(character(0)), 
                 "'object'[:] Must have length 1")
  }
)

test_that(
  "Return an error if the string provided doesn't have an approximate format", 
  {
    local_reproducible_output(width = 200)
    # To pass this test, the string must have at least one pipe (|)
    expect_error(fieldChoiceMapping("This only has, a comma", "test1"), NA)
    expect_error(fieldChoiceMapping("1,", "testNULL"), NA) # Allow for NULL choice
    expect_error(fieldChoiceMapping("This only has | a pipe", "test2"), NA) # allowing only pipes
    expect_error(fieldChoiceMapping("This has neither", "test3"), "'test3' choice string does not appear to be formatted for choices.")
  }
)

test_that(
  "Return an error if field_name is not in the metadata", 
  {
    local_reproducible_output(width = 200)
    expect_error(fieldChoiceMapping(rcon, "..Variable..not..found.."), 
                 "'..Variable..not..found..' is not a field listed in the meta data")
  }
)

test_that(
  "Return an error if the field_name is not a checkbox, dropdown, or radio", 
  {
    local_reproducible_output(width = 200)
    expect_error(fieldChoiceMapping(rcon, "record_id"), 
                 "'record_id' is not a checkbox, dropdown, or radio field")
  }
)

test_that(
  "Returns the correct mapping", 
  {
    load(test_path("testdata", "test_redcapAPI_MetaData.Rdata"))
    Meta <- test_redcapAPI_MetaData
    Meta <- Meta[Meta$field_name %in% c("record_id", "prereq_checkbox"), ]
    importMetaData(rcon, Meta)
    
    field_choice <- rcon$metadata()
    field_choice <- field_choice$select_choices_or_calculations[field_choice$field_name == "prereq_checkbox"]
    
    DesiredOutput <- 
      matrix(c("1", "Checkbox1", 
               "2", "Checkbox2", 
               "ABC", "CheckboxABC", 
               "4", "Do not use in branching logic"), 
             nrow = 4, 
             ncol = 2, 
             byrow = TRUE, 
             dimnames = list(NULL, c("choice_value", "choice_label")))
    
    expect_equal(fieldChoiceMapping(field_choice), 
                 DesiredOutput)
    expect_equal(fieldChoiceMapping(rcon, "prereq_checkbox"), 
                 DesiredOutput)
    
    importMetaData(rcon, Meta[1, ])
  }
)

test_that(
  "Returns the correct mapping when the label includes a comma", 
  {
    field_choice <- c("1, Backgammon | 2, Checkers, Chess")
    
    DesiredOutput <- 
      matrix(c("1", "Backgammon", 
               "2", "Checkers, Chess"), 
             nrow = 2, 
             ncol = 2, 
             byrow = TRUE, 
             dimnames = list(NULL, c("choice_value", "choice_label")))
    
    expect_equal(fieldChoiceMapping(field_choice), 
                 DesiredOutput)
  }
)

test_that(
  "Returns the correct mapping when the label is the same as the code",
  {
    field_choice <- c("Mr. | Mrs.")

    DesiredOutput <-
      matrix(c("Mr.", "Mr.",
               "Mrs.", "Mrs."),
             nrow = 2,
             ncol = 2,
             byrow = TRUE,
             dimnames = list(NULL, c("choice_value", "choice_label")))

    expect_equal(fieldChoiceMapping(field_choice),
                 DesiredOutput)
  }
)
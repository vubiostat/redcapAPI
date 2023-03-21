context("fieldChoiceMapping.R")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Return an error if object is neither character nor redcapApiConnection", 
  {
    expect_error(fieldChoiceMapping(mtcars), 
                 "no applicable method for 'fieldChoiceMapping'")
  }
)

test_that(
  "Return an error if the character string has length > 1", 
  {
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
    # To pass this test, the string must have at least one comma (,) and one pipe (|)
    expect_error(fieldChoiceMapping("This only has, a comma"), 
                 "The field choice string does not appear to be formatted for choices.")
    expect_error(fieldChoiceMapping("This only has | a pipe"), 
                 "The field choice string does not appear to be formatted for choices.")
    expect_error(fieldChoiceMapping("This has neither"), 
                 "The field choice string does not appear to be formatted for choices.")
  }
)

test_that(
  "Return an error if field_name is not in the metadata", 
  {
    expect_error(fieldChoiceMapping(rcon, "..Variable..not..found.."), 
                 "'..Variable..not..found..' is not a field listed in the meta data")
  }
)

test_that(
  "Return an error if the field_name is not a checkbox, dropdown, or radio", 
  {
    expect_error(fieldChoiceMapping(rcon, "record_id"), 
                 "'record_id' is not a checkbox, dropdown, or radio field")
  }
)

test_that(
  "Returns the correct mapping", 
  {
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

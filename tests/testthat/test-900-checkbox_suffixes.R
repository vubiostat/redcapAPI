context("checkbox_suffixes.R")

CheckboxMetaData <- exportMetaData(rcon)
CheckboxMetaData <- CheckboxMetaData[CheckboxMetaData$field_name %in% c("prereq_checkbox"), ]
# For the purpose of testing, we are going to add a couple more options to these meta data
# Doing it this way allows us to add tests for any code/label mapping without having to 
# alter the testing database.
CheckboxMetaData$select_choices_or_calculations <- 
  paste0(CheckboxMetaData$select_choices_or_calculations, 
         " | lowercase, Lowercase code | mixedCase, Mixed case code | 12ab, alpha, numeric | use_underscore, Use an underscore")

test_that(
  "Checkbox suffixes are correctly generated", 
  {
    expect_equal(
      checkbox_suffixes(fields = c("prereq_checkbox"), 
                        meta_data = CheckboxMetaData), 
      list(name_suffix = c(prereq_checkbox1 = "prereq_checkbox___1", 
                           prereq_checkbox2 = "prereq_checkbox___2", 
                           prereq_checkbox3 = "prereq_checkbox___abc", 
                           prereq_checkbox4 = "prereq_checkbox___4", 
                           prereq_checkbox5 = "prereq_checkbox___lowercase", 
                           prereq_checkbox6 = "prereq_checkbox___mixedcase", 
                           prereq_checkbox7 = "prereq_checkbox___12ab", 
                           prereq_checkbox8 = "prereq_checkbox___use_underscore"), 
           label_suffix = c(prereq_checkbox1 = "Pre-requisite as a checkbox: Checkbox1", 
                            prereq_checkbox2 = "Pre-requisite as a checkbox: Checkbox2",
                            prereq_checkbox3 = "Pre-requisite as a checkbox: CheckboxABC", 
                            prereq_checkbox4 = "Pre-requisite as a checkbox: Do not use in branching logic", 
                            prereq_checkbox5 = "Pre-requisite as a checkbox: Lowercase code", 
                            prereq_checkbox6 = "Pre-requisite as a checkbox: Mixed case code", 
                            prereq_checkbox7 = "Pre-requisite as a checkbox: alpha, numeric", 
                            prereq_checkbox8 = "Pre-requisite as a checkbox: Use an underscore"))
    )
  }
)

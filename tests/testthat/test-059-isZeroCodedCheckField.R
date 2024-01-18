context("isZeroCodedCheckField Functionality (and warnings)")

#####################################################################
# isZeroCodedCheckField                                          ####

test_that(
  "test isZeroCodedCheckField",
  {
    expect_true(isZeroCodedCheckField("checkbox___0"))
    expect_true(isZeroCodedCheckField("some_field_name___0"))
    expect_false(isZeroCodedCheckField("four_underscore____0"))
    expect_false(isZeroCodedCheckField("checkbox___0_"))
    expect_false(isZeroCodedCheckField("checkbox___1"))
    expect_false(isZeroCodedCheckField("checkbox___a"))
    expect_false(isZeroCodedCheckField("checkbox___00"))
  }
)

test_that(
  "Return an error if field_name is not character(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(isZeroCodedCheckField(0))
    expect_error(isZeroCodedCheckField(c("checkbox___0", 
                                         "checkbox___1")))
  }
)

#####################################################################
# warnOfZeroCodedCheckCasting                                    ####

test_that(
  "Return a warning when casting a zero-coded field likely to fail", 
  {
    local_reproducible_output(width = 200)
    check <- factor(c(0, 1, 0, 1), 
                    0:1, 
                    c("", "0"))
    expect_warning(warnOfZeroCodedCheckCasting(field_name = "check_zero___0", 
                                               x = check), 
                   "Zero-coded check field .+ may not have been cast correctly")
    
    expect_silent(warnOfZeroCodedCheckCasting(field_name = "check_zero___0", 
                                              factor(letters)))
    expect_silent(warnOfZeroCodedCheckCasting(field_name = "check_zero___0", 
                                              0:10))
    expect_silent(warnOfZeroCodedCheckCasting(field_name = "check_zero___1", 
                                              x = check))
  }
)

test_that(
  "warnOfZeroCodedCheckCasting argument validation", 
  {
    local_reproducible_output(width = 200)
    expect_error(warnOfZeroCodedCheckCasting(field_name = 1, 
                                             x = 1:3), 
                 "'field_name': Must be of type 'character'")
    expect_error(warnOfZeroCodedCheckCasting(field_name = letters[1:2], 
                                             x = 1:3), 
                 "'field_name': Must have length 1")
    
    expect_error(warnOfZeroCodedCheckCasting(field_name = "check_zero___0", 
                                             x = mtcars), 
                 "'x': Must be of type 'atomic'")
  }
)

#####################################################################
# warnZeroCodedFieldPresent                                      ####

test_that(
  "Issue warning when zero-coded check fields are present", 
  {
    local_reproducible_output(width = 200)
    expect_warning(
      warnZeroCodedFieldPresent(field_names = c("checkbox_zero___0", 
                                                "checkbox_zero___1"),
                                TRUE), 
      "Zero-coded check fields found. Verify that casting is correct for checkbox_zero___0")
    
    expect_warning(
      warnZeroCodedFieldPresent(field_names = c("checkbox_zero___0", 
                                                "checkbox_other___0"),
                                TRUE), 
      "Zero-coded check fields found. Verify that casting is correct for checkbox_zero___0, checkbox_other___0")
    
    expect_silent(
      warnZeroCodedFieldPresent(field_names = "field_name",
                                TRUE)
    )
    
    expect_silent(
      warnZeroCodedFieldPresent(field_names = c("checkbox_zero___0", 
                                                "checkbox_other___0"),
                                warn_zero_coded=FALSE)
    )
    
    expect_silent(
      warnZeroCodedFieldPresent(field_names = c("checkbox_zero___0", 
                                                "checkbox_zero___1"),
                                FALSE)
    )
  }
)

test_that(
  "warnZeroCodeFieldPresent argument validation", 
  {
    local_reproducible_output(width = 200)
    expect_error(
      warnZeroCodedFieldPresent(field_names = 1:3,TRUE), 
      "'field_names': Must be of type 'character'"
    )
    
    expect_error(
      warnZeroCodedFieldPresent(field_names = "yyz","yyz"), 
      "'warn_zero_coded': Must be of type 'logical'"
    )
    
    expect_error(
      warnZeroCodedFieldPresent(field_names = "yyz",NA), 
      "Variable 'warn_zero_coded': Contains missing values"
    )
    
    expect_error(
      warnZeroCodedFieldPresent(field_names = "yyz",logical()), 
      "Variable 'warn_zero_coded': Must have length 1"
    )
    
    expect_error(
      warnZeroCodedFieldPresent(field_names = "yyz",c(TRUE,TRUE)), 
      "Variable 'warn_zero_coded': Must have length 1"
    )
  }
)

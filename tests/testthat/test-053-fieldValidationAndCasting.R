context("Field Validation And Casting Functions")

#####################################################################
# isNAorBlank                                                    ####

test_that(
  "isNAorBlank", 
  {
    expect_equal(isNAorBlank(c("", NA, 1, 2, "abc", " ", "|")), 
                 c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  }
)

test_that(
  "Default datetime validations",
  {
    default <- redcapAPI:::.default_validate
    # date_
    date_to_validate <- c("2023-04-01", 
                          "2023-13-01", 
                          "2023-04-31",
                          "2023-00-17", 
                          "23-01-01")
    expect_equal(default$date_(date_to_validate), 
                 c(TRUE, FALSE, FALSE, FALSE, TRUE))
    
    #datetime_  
    datetime_to_validate <- c("2023-04-01 00:00",
                              "2023-04-31 10:30",
                              "2023-04-01 25:00", 
                              "2023-04-01 12:60", 
                              "2023-04 01 12:00", 
                              "2023-04-01 12 00") 
    expect_equal(default$datetime_(datetime_to_validate), 
                 c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
    
    # datetime_seconds_  
    datetime_to_validate <- c("2023-04-01 00:00:00", 
                              "2023-02-30 00:00:00",
                              "2023-04-01 25:00:00", 
                              "2023-04-01 12:60:00", 
                              "2023-04-01 12:00:61", 
                              "2023-04 01 12:00:00", 
                              "2023-04-01 12 00:00", 
                              "2023-04-01 12:00 00")
    expect_equal(default$datetime_seconds_(datetime_to_validate),
                 c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  }
)

#####################################################################
# valRx                                                          ####

test_that(
  "valRx", 
  {
    # date_
    date_to_validate <- c("2023-04-01", 
                          "2023-13-01", 
                          "2023-04-31",
                          "2023-00-17", 
                          "23-01-01")
    expect_equal(valRx(REGEX_DATE)(date_to_validate), 
                 c(TRUE, FALSE, TRUE, FALSE, TRUE))
    
    #datetime_  
    datetime_to_validate <- c("2023-04-01 00:00", 
                              "2023-04-01 25:00", 
                              "2023-04-01 12:60", 
                              "2023-04 01 12:00", 
                              "2023-04-01 12 00") 
    expect_equal(valRx(REGEX_DATETIME)(datetime_to_validate), 
                 c(TRUE, FALSE, FALSE, FALSE, FALSE))
    
    # datetime_seconds_  
    datetime_to_validate <- c("2023-04-01 00:00:00", 
                              "2023-04-01 25:00:00", 
                              "2023-04-01 12:60:00", 
                              "2023-04-01 12:00:60", 
                              "2023-04 01 12:00:00", 
                              "2023-04-01 12 00:00", 
                              "2023-04-01 12:00 00")
    expect_equal(valRx(REGEX_DATETIME_SECONDS)(datetime_to_validate),
                 c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
    
    # time_mm_ss     
    time_to_validate <- c("03:59", 
                          "00:12", 
                          "1:3", 
                          "01:60", 
                          "60:23", 
                          "12 00")
    expect_equal(
      valRx(REGEX_TIME_MMSS)(time_to_validate), 
      c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
    )
    
    #time_hh_mm_ss 
    time_to_validate <- c("03:59:00", 
                          "00:12:00", 
                          "1:3:30", 
                          "01:60:59", 
                          "60:23:12",
                          "12 00:37", 
                          "12:45:60", 
                          "12:45 10", 
                          "12:45:9") 
    expect_equal(
      valRx(REGEX_TIME_HHMMSS)(time_to_validate), 
      c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
    )
    
    # time
    time_to_validate <- c("6:23", 
                          "24:50",
                          "12:60", 
                          "13 11")
    expect_equal(
      valRx(REGEX_TIME)(time_to_validate), 
      c(TRUE, FALSE, FALSE, FALSE)
    )
    
    # float              
    float_to_validate <- c("12.345", 
                           "123", 
                           "9923", 
                           "99.", 
                           "1.0", 
                           "1E9", 
                           "9e1", 
                           "3e-3", 
                           "2.a") 
    expect_equal(
      valRx(REGEX_FLOAT)(float_to_validate), 
      c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
    )
    
    # number
    to_validate <- c("12.345", 
                     "123", 
                     "9923", 
                     "99.", 
                     "1.0", 
                     "1E9", 
                     "9e1", 
                     "3e-3", 
                     "2.a")  
    expect_equal(
      valRx(REGEX_NUMBER)(to_validate), 
      c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
    )
    
    # number with comma as decimal place
    to_validate <- c("12,345", 
                     "123", 
                     "9923", 
                     "99.", 
                     "1,0", 
                     "1E9", 
                     "9e1", 
                     "3e-3", 
                     "2,a")  
    expect_equal(
      valRx(REGEX_NUMBER_COMMA)(to_validate), 
      c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE)
    )
    
    # int    
    int_to_validate <- c("1", 
                         "1.0", 
                         "1.00", 
                         "1.1",
                         "123.000", 
                         "2.001", 
                         "19.00000000000000003")
    expect_equal(
      valRx(REGEX_INT)(int_to_validate), 
      c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
    )
    
    # integer
    # The integer regex differs from the int regex, in that it does not 
    # allow for a decimal point.
    integer_to_validate <-c("1", 
                            "1.0", 
                            "1.00", 
                            "1.1",   
                            "123.000", 
                            "2.001", 
                            "19.00000000000000003") 
    expect_equal(valRx(REGEX_INTEGER)(integer_to_validate), 
                 c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
    
    # yesno              
    yesno_to_validate <- c("yes", 
                           "no", 
                           "0", 
                           "1", 
                           "01", 
                           "00", 
                           "Yes", 
                           "NO", 
                           "3",
                           "-")
    expect_equal(
      valRx(REGEX_YES_NO)(yesno_to_validate), 
      c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)
    )
    
    # truefalse         
    truefalse_to_validate <- c("true", 
                               "false", 
                               "TrUe",
                               "FalSE",
                               "0", 
                               "1", 
                               "00",     
                               "01",     
                               "3", 
                               "-")
    expect_equal(
      valRx(REGEX_TRUE_FALSE)(truefalse_to_validate), 
      c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
    )
    
    # checkbox 
    checkbox_to_validate <- c("0", 
                              "1", 
                              "yes", 
                              "no", 
                              "checked", 
                              "unchecked", 
                              "Yes", 
                              "nO", 
                              "chEcked", 
                              "unchecKed", 
                              "other")
    expect_equal(
      valRx(REGEX_CHECKBOX)(checkbox_to_validate), 
      c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
    )
    
    # form_complete
    form_to_validate <- c("0", 
                          "1", 
                          "2", 
                          "3", 
                          "-", 
                          "a")
    expect_equal(
      valRx("[012]")(form_to_validate), 
      c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
    )
  }
)

#####################################################################
# valChoice                                                      ####

test_that(
  "valChoice gives expected results", 
  {
    coding <- c("Apple" = "1", "Banana" = "2")
    expect_equal(valChoice(c("1", "2", "3", "apple", "banana", "Apple"), 
                           field_name = "some_field", 
                           coding = coding), 
                 c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE))
  }
)

#####################################################################
# valSkip                                                        ####

test_that(
  "valSkip gives expected results",
  {
     coding <- c("x" = "Ex", y = "Why", z = "Zee")
     field_name <- "some_field"
     x <- rep(FALSE, 5)
     
     expect_equal(valSkip(x, field_name, coding), 
                  rep(TRUE, 5))
  }
)

#####################################################################
# getCodingIndex                                                 ####

test_that(
  "getCodingIndex", 
  {
    coding <- c("Peanut" = 1, 
                "Walnut" = "b", 
                "Cashew" = "xyz", 
                "Almond" = -4, 
                "Pecan"  = "ABC")
    
    x <- c("1", "b", "xyz", "-4", "ABC",
           "Peanut", "Walnut", "Cashew", "Almond", "Pecan", 
           "Anything else")
    
    expect_equal(getCodingIndex(x, coding), 
                 c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, NA))
  }
)

#####################################################################
# getCheckedValue                                                ####

test_that(
  "getCheckedValue", 
  {
    coding <- c("Peanut" = 1, 
                "Walnut" = "b", 
                "Cashew" = "xyz", 
                "Almond" = -4, 
                "Pecan"  = "ABC")
    
    expect_true(all(getCheckedValue(coding, field_name = "checkbox___1") %in% 
                      c("1", "Peanut", "1", "Checked", "yes")))
    expect_true(all(getCheckedValue(coding, field_name = "checkbox___b") %in% 
                      c("b", "Walnut", "1", "Checked", "yes")))
    expect_true(all(getCheckedValue(coding, field_name = "checkbox___xyz") %in% 
                      c("xyz", "Cashew", "1", "Checked", "yes")))
    expect_true(all(getCheckedValue(coding, field_name = "checkbox____4") %in%
                      c("-4", "Almond", "1", "Checked", "yes")))
    expect_true(all(getCheckedValue(coding, field_name = "checkbox___abc") %in%
                      c("ABC", "Pecan", "1", "Checked", "yes")))
  }
)

#####################################################################
# coerceNumericIfAble                                            ####

test_that(
  "Coerce to numeric if possible", 
  {
    expect_numeric(coerceNumericIfAble(1:3))
    expect_numeric(coerceNumericIfAble(as.character(pi)))
    expect_character(coerceNumericIfAble(letters))
    expect_data_frame(coerceNumericIfAble(mtcars))
  }
)

#####################################################################
# castLabel                                                    ####

test_that(
  "Cast a field to labels", 
  {
    coding <- c("Peanut" = 1, 
                "Walnut" = "b", 
                "Cashew" = "xyz", 
                "Almond" = -4, 
                "Pecan"  = "ABC")
    x <- c("Peanut", "1", 
           "Walnut", "b", 
           "Cashew", "xyz", 
           "Almond", "-4",
           "Pecan",  "ABC",
           "something else")
    
    expect_equal(castLabel(x, "field_name", coding), 
                 factor(c("Peanut", "Peanut", "Walnut", "Walnut", 
                          "Cashew", "Cashew", "Almond", "Almond",
                          "Pecan", "Pecan",
                          NA), 
                        levels = c("Peanut", "Walnut", "Cashew", "Almond", "Pecan")))
  }
)

test_that(
  "Cast a field to labels (character output)", 
  {
    coding <- c("Peanut" = 1, 
                "Walnut" = "b", 
                "Cashew" = "xyz", 
                "Almond" = -4, 
                "Pecan"  = "ABC")
    x <- c("Peanut", "1", 
           "Walnut", "b", 
           "Cashew", "xyz", 
           "Almond", "-4",
           "Pecan",  "ABC",
           "something else")
    
    expect_equal(castLabelCharacter(x, "field_name", coding), 
                 c("Peanut", "Peanut", "Walnut", "Walnut", 
                   "Cashew", "Cashew", "Almond", "Almond",
                   "Pecan", "Pecan",
                   NA))
  }
)

#####################################################################
# castCode                                                     ####

test_that(
  "Cast a field to codes", 
  {
    coding <- c("Peanut" = 1, 
                "Walnut" = "b", 
                "Cashew" = "xyz", 
                "Almond" = -4, 
                "Pecan"  = "ABC")
    x <- c("Peanut", "1", 
           "Walnut", "b", 
           "Cashew", "xyz", 
           "Almond", "-4",
           "Pecan",  "ABC",
           "something else")
    
    expect_equal(castCode(x, "field_name", coding), 
                 factor(c("1", "1", "b", "b", "xyz", "xyz", "-4", "-4", "ABC", "ABC", 
                          NA), 
                        levels = c("1", "b", "xyz", "-4", "ABC")))
  }
)

test_that(
  "Cast a field to codes (character output)", 
  {
    coding <- c("Peanut" = 1, 
                "Walnut" = "b", 
                "Cashew" = "xyz", 
                "Almond" = -4, 
                "Pecan"  = "ABC")
    x <- c("Peanut", "1", 
           "Walnut", "b", 
           "Cashew", "xyz", 
           "Almond", "-4",
           "Pecan",  "ABC",
           "something else")
    
    expect_equal(castCodeCharacter(x, "field_name", coding), 
                 c("1", "1", "b", "b", "xyz", "xyz", "-4", "-4", "ABC", "ABC", 
                          NA))
  }
)

#####################################################################
# castRaw                                                      ####

test_that(
  "Cast a field to raw", 
  {
    coding <- c("Peanut" = 1, 
                "Walnut" = "b", 
                "Cashew" = "xyz", 
                "Almond" = -4, 
                "Pecan"  = "ABC")
    x <- c("Peanut", "1", 
           "Walnut", "b", 
           "Cashew", "xyz", 
           "Almond", "-4",
           "Pecan",  "ABC",
           "something else")
    
    expect_equal(castRaw(x, "field_name", coding), 
                 c("1", "1", "b", "b", "xyz", "xyz", "-4", "-4", "ABC", "ABC",
                   NA))
    
    # and coerce to numeric
    coding <- c("Peanut" = 1, 
                "Walnut" = 2, 
                "Cashew" = 3, 
                "Almond" = -4, 
                "Pecan"  = 0)
    x <- c("Peanut", "1", "Walnut", "2", "Cashew", "3", "Almond", "-4", "Pecan", "Pecan", "something else")
    
    expect_equal(castRaw(x, "field_name", coding), 
                 c(1, 1, 2, 2, 3, 3, -4, -4, 0, 0, 
                   NA))
    
    
    # A checkbox with missing values preserves the missing values (See Issue 228)
    x <- c("0", NA, "1")
    coding <- c(Guitar = "x", Ukulele = "y", Mandolin = "z")
    expect_equal(castRaw(x, "field___x", coding), 
                 c(0, NA, 1))
  }
)

#####################################################################
# castChecked                                                  ####

test_that(
  "Cast a checkbox to Checked/Unchecked", 
  {
    coding <- c("Peanut" = 1, 
                "Walnut" = "b", 
                "Cashew" = "xyz", 
                "Almond" = -4, 
                "Pecan"  = "ABC")
    x <- c("Peanut", "1", "Walnut", "b", "Cashew", "xyz", "Almond", "-4", "Pecan", "ABC", "something else")
    
    expect_equal(as.character(castChecked(x, "checkbox___1", coding)), 
                 c("Checked",   "Checked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked"))
    expect_equal(as.character(castChecked(x, "checkbox___b", coding)), 
                 c("Unchecked", "Checked", 
                   "Checked",   "Checked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked"))
    expect_equal(as.character(castChecked(x, "checkbox___xyz", coding)), 
                 c("Unchecked", "Checked", 
                   "Unchecked", "Unchecked", 
                   "Checked",   "Checked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked"))
    expect_equal(as.character(castChecked(x, "checkbox____4", coding)), 
                 c("Unchecked", "Checked", 
                   "Unchecked", "Unchecked",
                   "Unchecked", "Unchecked", 
                   "Checked",   "Checked",  
                   "Unchecked", "Unchecked", 
                   "Unchecked"))
    expect_equal(as.character(castChecked(x, "checkbox___abc", coding)), 
                 c("Unchecked", "Checked", 
                   "Unchecked", "Unchecked",
                   "Unchecked", "Unchecked",  
                   "Unchecked", "Unchecked",
                   "Checked",   "Checked",  
                   "Unchecked"))
  }
)

test_that(
  "Cast a checkbox to Checked/Unchecked (character output)", 
  {
    coding <- c("Peanut" = 1, 
                "Walnut" = "b", 
                "Cashew" = "xyz", 
                "Almond" = -4, 
                "Pecan"  = "ABC")
    x <- c("Peanut", "1", "Walnut", "b", "Cashew", "xyz", "Almond", "-4", "Pecan", "ABC", "something else")
    
    expect_equal(castCheckedCharacter(x, "checkbox___1", coding), 
                 c("Checked",   "Checked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked"))
    expect_equal(castCheckedCharacter(x, "checkbox___b", coding), 
                 c("Unchecked", "Checked", 
                   "Checked",   "Checked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked"))
    expect_equal(castCheckedCharacter(x, "checkbox___xyz", coding), 
                 c("Unchecked", "Checked", 
                   "Unchecked", "Unchecked", 
                   "Checked",   "Checked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked"))
    expect_equal(castCheckedCharacter(x, "checkbox____4", coding), 
                 c("Unchecked", "Checked", 
                   "Unchecked", "Unchecked",
                   "Unchecked", "Unchecked", 
                   "Checked",   "Checked",  
                   "Unchecked", "Unchecked", 
                   "Unchecked"))
    expect_equal(castCheckedCharacter(x, "checkbox___abc", coding), 
                 c("Unchecked", "Checked", 
                   "Unchecked", "Unchecked",
                   "Unchecked", "Unchecked",  
                   "Unchecked", "Unchecked",
                   "Checked",   "Checked",  
                   "Unchecked"))
  }
)

#####################################################################
# castCheckLabel                                               ####

test_that(
  "cast to labelled checkbox", 
  {
    coding <- c("Peanut" = 1, 
                "Walnut" = "b", 
                "Cashew" = "xyz", 
                "Almond" = -4, 
                "Pecan"  = "ABC")
    x <- c("Peanut", "1", "Walnut", "b", "Cashew", "xyz", "Almond", "-4", "Pecan", "ABC", "something else")
    
    expect_equal(as.character(castCheckLabel(x, "checkbox___1", coding)), 
                 c("Peanut",   "Peanut", 
                   "", "", 
                   "", "", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckLabel(x, "checkbox___b", coding)), 
                 c("", "Walnut", 
                   "Walnut",   "Walnut", 
                   "", "", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckLabel(x, "checkbox___xyz", coding)), 
                 c("", "Cashew", 
                   "", "", 
                   "Cashew",   "Cashew", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckLabel(x, "checkbox____4", coding)), 
                 c("", "Almond", 
                   "", "",
                   "", "", 
                   "Almond",   "Almond",  
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckLabel(x, "checkbox___abc", coding)), 
                 c("", "Pecan", 
                   "", "",
                   "", "",  
                   "", "",
                   "Pecan",   "Pecan",  
                   ""))
  }
)

test_that(
  "cast to labelled checkbox as character (character otuput)", 
  {
    coding <- c("Peanut" = 1, 
                "Walnut" = "b", 
                "Cashew" = "xyz", 
                "Almond" = -4, 
                "Pecan"  = "ABC")
    x <- c("Peanut", "1", "Walnut", "b", "Cashew", "xyz", "Almond", "-4", "Pecan", "ABC", "something else")
    
    expect_equal(castCheckLabelCharacter(x, "checkbox___1", coding), 
                 c("Peanut",   "Peanut", 
                   "", "", 
                   "", "", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(castCheckLabelCharacter(x, "checkbox___b", coding), 
                 c("", "Walnut", 
                   "Walnut",   "Walnut", 
                   "", "", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(castCheckLabelCharacter(x, "checkbox___xyz", coding), 
                 c("", "Cashew", 
                   "", "", 
                   "Cashew",   "Cashew", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(castCheckLabelCharacter(x, "checkbox____4", coding), 
                 c("", "Almond", 
                   "", "",
                   "", "", 
                   "Almond",   "Almond",  
                   "", "", 
                   ""))
    expect_equal(castCheckLabelCharacter(x, "checkbox___abc", coding), 
                 c("", "Pecan", 
                   "", "",
                   "", "",  
                   "", "",
                   "Pecan",   "Pecan",  
                   ""))
  }
)

#####################################################################
# castCheckCode                                                ####

test_that(
  "cast to coded checkbox", 
  {
    coding <- c("Peanut" = 1, 
                "Walnut" = "b", 
                "Cashew" = "xyz", 
                "Almond" = -4, 
                "Pecan"  = "ABC")
    x <- c("Peanut", "1", "Walnut", "b", "Cashew", "xyz", "Almond", "-4", "Pecan", "ABC", "something else")
    
    expect_equal(as.character(castCheckCode(x, "checkbox___1", coding)), 
                 c("1",   "1", 
                   "", "", 
                   "", "", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckCode(x, "checkbox___b", coding)), 
                 c("", "b", 
                   "b",   "b", 
                   "", "", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckCode(x, "checkbox___xyz", coding)), 
                 c("", "xyz", 
                   "", "", 
                   "xyz",   "xyz", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckCode(x, "checkbox____4", coding)), 
                 c("", "-4", 
                   "", "",
                   "", "", 
                   "-4",   "-4",  
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckCode(x, "checkbox___abc", coding)), 
                 c("", "ABC", 
                   "", "",
                   "", "",  
                   "", "",
                   "ABC",   "ABC",  
                   ""))
  }
)

test_that(
  "cast to coded checkbox (character output)", 
  {
    coding <- c("Peanut" = 1, 
                "Walnut" = "b", 
                "Cashew" = "xyz", 
                "Almond" = -4, 
                "Pecan"  = "ABC")
    x <- c("Peanut", "1", "Walnut", "b", "Cashew", "xyz", "Almond", "-4", "Pecan", "ABC", "something else")
    
    expect_equal(castCheckCodeCharacter(x, "checkbox___1", coding), 
                 c("1",   "1", 
                   "", "", 
                   "", "", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(castCheckCodeCharacter(x, "checkbox___b", coding), 
                 c("", "b", 
                   "b",   "b", 
                   "", "", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(castCheckCodeCharacter(x, "checkbox___xyz", coding), 
                 c("", "xyz", 
                   "", "", 
                   "xyz",   "xyz", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(castCheckCodeCharacter(x, "checkbox____4", coding), 
                 c("", "-4", 
                   "", "",
                   "", "", 
                   "-4",   "-4",  
                   "", "", 
                   ""))
    expect_equal(castCheckCodeCharacter(x, "checkbox___abc", coding), 
                 c("", "ABC", 
                   "", "",
                   "", "",  
                   "", "",
                   "ABC",   "ABC",  
                   ""))
  }
)

#####################################################################
# castCheckForImport                                             ####

test_that(
  "castCheckForImport", 
  {
    x <- c("a", "b", "c", "d", "1", "Checked", "0")
    
    expect_equal(castCheckForImport()(x), 
                 c(0, 0, 0, 0, 1, 1, 0))
    
    expect_equal(castCheckForImport(c("b", "c"))(x), 
                 c(0, 1, 1, 0, 0, 0, 0))
    
    expect_equal(castCheckForImport("0")(x), 
                 c(0, 0, 0, 0, 0, 0, 1))
    
    # preserve NA values
    
    x[c(2, 4)] <- rep(NA, 2)
    
    expect_equal(castCheckForImport()(x), 
                 c(0, NA, 0, NA, 1, 1, 0))
  }
)

#####################################################################
# castLogical                                                    ####

test_that(
  "castLogical",
  {
    x <- c("0", "1", "false", "true", "no", "yes")
    
    expect_equal(castLogical(x), 
                 c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))
    
    expect_equal(castLogical(toupper(x)), 
                 c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
    
    # Preserves NA
    x <- c("0", NA_character_, "1")
    expect_equal(castLogical(x), 
                 c(FALSE, NA, TRUE))
  }
)

#####################################################################
# na_values Testing                                              ####

test_that(
  "na_values returns the desired lists", 
  {
    local_reproducible_output(width = 200)
    use_na_or_blank <- na_values(isNAorBlank)
    expect_true(all(vapply(use_na_or_blank, 
                           function(x) identical(x, isNAorBlank), 
                           logical(1))))
    expect_equal(names(use_na_or_blank), 
                 FIELD_TYPES)
    
    
    
    use_sum <- na_values(sum)
    expect_true(all(vapply(use_sum, 
                           function(x) identical(x, sum), 
                           logical(1))))
    expect_equal(names(use_sum), 
                 FIELD_TYPES)
    
    
    
    # Return an error if the FUN is not a function
    
    expect_error(na_values(mtcars), 
                 "Variable 'FUN': Must be a function")
  }
)

context("fieldValidationAndCasting.R")

#####################################################################
# isNAorBlank                                                    ####

test_that(
  "isNAorBlank", 
  {
    expect_equal(isNAorBlank(c("", NA, 1, 2, "abc", " ", "|")), 
                 c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
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
                          "2023-04-31",  # FIXME This isn't really a valid date 
                          "2023-00-17", 
                          "23-01-01")
    expect_equal(valRx("[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])")(date_to_validate), 
                 c(TRUE, FALSE, TRUE, FALSE, TRUE))
    
    #datetime_  
    datetime_to_validate <- c("2023-04-01 00:00", 
                              "2023-04-01 25:00", 
                              "2023-04-01 12:60", 
                              "2023-04 01 12:00", 
                              "2023-04-01 12 00") 
    expect_equal(valRx("[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])\\s([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]")(datetime_to_validate), 
                 c(TRUE, FALSE, FALSE, FALSE, FALSE))
    
    # datetime_seconds_  
    datetime_to_validate <- c("2023-04-01 00:00:00", 
                              "2023-04-01 25:00:00", 
                              "2023-04-01 12:60:00", 
                              "2023-04-01 12:00:60", 
                              "2023-04 01 12:00:00", 
                              "2023-04-01 12 00:00", 
                              "2023-04-01 12:00 00")
    expect_equal(
      valRx("[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])\\s([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]")(datetime_to_validate),
      c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
    )
    
    # time_mm_ss     
    time_to_validate <- c("03:59", 
                          "00:12", 
                          "1:3", 
                          "01:60", 
                          "60:23", 
                          "12 00")
    expect_equal(
      valRx("[0-5][0-9]:[0-5][0-9]")(time_to_validate), 
      c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
    )
    
    #time_hh_mm_ss 
    time_to_validate <- c("03:59:00", 
                          "00:12:00", 
                          "1:3:30", 
                          "01:60:59", 
                          "60:23:12",  # FIXME: This isn't actually a time. Do we need to enforce start and end of string?
                          "12 00:37", 
                          "12:45:60", 
                          "12:45 10", 
                          "12:45:9") 
    expect_equal(
      valRx("([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]")(time_to_validate), 
      c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
    )
    
    # time
    time_to_validate <- c("6:23", 
                          "24:50",   # FIXME: This isn't a valid time. It passes because we dont enfoce the start of string
                          "12:60", 
                          "13 11")
    expect_equal(
      valRx("([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]")(time_to_validate), 
      c(TRUE, TRUE, FALSE, FALSE)
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
                           "2.a")  # FIXME: this isn't a valid float
    expect_equal(
      valRx("[-+]?(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))([Ee][+-]?[0-9]+)?")(float_to_validate), 
      c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
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
                     "2.a")  # FIXME: this isn't a valid number
    expect_equal(
      valRx("[-+]?(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))([Ee][+-]?[0-9]+)?")(to_validate), 
      c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
    )
    
    # This is the same REGEX used for float and number.  Should we make this a constant?
    # calc               
    #valRx("[-+]?(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))([Ee][+-]?[0-9]+)?")
    
    # int    
    int_to_validate <- c("1", 
                         "1.0", 
                         "1.00", 
                         "1.1",
                         "123.000", 
                         "2.001", 
                         "19.00000000000000003")
    expect_equal(
      valRx("^[-+]?[0-9]+(|\\.|\\.[0]+)$")(int_to_validate), 
      c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
    )
    
    # integer
    integer_to_validate <-c("1", 
                            "1.0", 
                            "1.00", 
                            "1.1",   # FIXME: not an integer
                            "123.000", 
                            "2.001", # FIXME: not an integer
                            "19.00000000000000003")  # FIXME: not an integer
    expect_equal(valRx("[-+]?[0-9]+")(integer_to_validate), 
                 c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
    
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
      valRx("^(?i)(0|1|yes|no)$")(yesno_to_validate), 
      c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)
    )
    
    # truefalse         
    truefalse_to_validate <- c("true", 
                               "false", 
                               "TrUe",   # FIXME: do we want to ignore case
                               "FalSE",  # FIXME: do we want to ignore case
                               "0", 
                               "1", 
                               "00",     # FIXME: Should we enfoce start and end of string?
                               "01",     # FIXME: Should we enfoce start and end of string?
                               "3", 
                               "-")
    expect_equal(
      valRx("(0|1|true|false)")(truefalse_to_validate), 
      c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
    )
    
    # FIXME: this is the same regex as yesno. Should the be made a constant?
    # checkbox           
    # valRx("^(?i)(0|1|yes|no)$")
    
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
                 c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
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
    
    expect_equal(castLabel(x, coding, "field_name"), 
                 factor(c("Peanut", "Peanut", "Walnut", "Walnut", 
                          "Cashew", "Cashew", "Almond", "Almond",
                          "Pecan", "Pecan",
                          NA), 
                        levels = c("Peanut", "Walnut", "Cashew", "Almond", "Pecan")))
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
    
    expect_equal(castCode(x, coding, "field_name"), 
                 factor(c("1", "1", "b", "b", "xyz", "xyz", "-4", "-4", "ABC", "ABC", 
                          NA), 
                        levels = c("1", "b", "xyz", "-4", "ABC")))
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
    
    expect_equal(castRaw(x, coding, "field_name"), 
                 c("1", "1", "b", "b", "xyz", "xyz", "-4", "-4", "ABC", "ABC",
                   NA))
    
    # and coerce to numeric
    coding <- c("Peanut" = 1, 
                "Walnut" = 2, 
                "Cashew" = 3, 
                "Almond" = -4, 
                "Pecan"  = 0)
    x <- c("Peanut", "1", "Walnut", "2", "Cashew", "3", "Almond", "-4", "Pecan", "Pecan", "something else")
    
    expect_equal(castRaw(x, coding, "field_name"), 
                 c(1, 1, 2, 2, 3, 3, -4, -4, 0, 0, 
                   NA))
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
    
    expect_equal(as.character(castChecked(x, coding, "checkbox___1")), 
                 c("Checked",   "Checked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked"))
    expect_equal(as.character(castChecked(x, coding, "checkbox___b")), 
                 c("Unchecked", "Checked", 
                   "Checked",   "Checked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked"))
    expect_equal(as.character(castChecked(x, coding, "checkbox___xyz")), 
                 c("Unchecked", "Checked", 
                   "Unchecked", "Unchecked", 
                   "Checked",   "Checked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked", "Unchecked", 
                   "Unchecked"))
    expect_equal(as.character(castChecked(x, coding, "checkbox____4")), 
                 c("Unchecked", "Checked", 
                   "Unchecked", "Unchecked",
                   "Unchecked", "Unchecked", 
                   "Checked",   "Checked",  
                   "Unchecked", "Unchecked", 
                   "Unchecked"))
    expect_equal(as.character(castChecked(x, coding, "checkbox___abc")), 
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
    
    expect_equal(as.character(castCheckLabel(x, coding, "checkbox___1")), 
                 c("Peanut",   "Peanut", 
                   "", "", 
                   "", "", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckLabel(x, coding, "checkbox___b")), 
                 c("", "Walnut", 
                   "Walnut",   "Walnut", 
                   "", "", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckLabel(x, coding, "checkbox___xyz")), 
                 c("", "Cashew", 
                   "", "", 
                   "Cashew",   "Cashew", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckLabel(x, coding, "checkbox____4")), 
                 c("", "Almond", 
                   "", "",
                   "", "", 
                   "Almond",   "Almond",  
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckLabel(x, coding, "checkbox___abc")), 
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
    
    expect_equal(as.character(castCheckCode(x, coding, "checkbox___1")), 
                 c("1",   "1", 
                   "", "", 
                   "", "", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckCode(x, coding, "checkbox___b")), 
                 c("", "b", 
                   "b",   "b", 
                   "", "", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckCode(x, coding, "checkbox___xyz")), 
                 c("", "xyz", 
                   "", "", 
                   "xyz",   "xyz", 
                   "", "", 
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckCode(x, coding, "checkbox____4")), 
                 c("", "-4", 
                   "", "",
                   "", "", 
                   "-4",   "-4",  
                   "", "", 
                   ""))
    expect_equal(as.character(castCheckCode(x, coding, "checkbox___abc")), 
                 c("", "ABC", 
                   "", "",
                   "", "",  
                   "", "",
                   "ABC",   "ABC",  
                   ""))
  }
)


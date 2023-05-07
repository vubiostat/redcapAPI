context("castForImport.R")

load(test_path("testdata", "RedcapProject_BasicData.Rdata"))

purgeProject(rcon, purge_all = TRUE)
rcon$flush_all()
restoreProject(RedcapProject_BasicData, rcon)
rcon$flush_all()

load(test_path("testdata", "BasicData.Rdata"))

#####################################################################
# Correct validation of field types

test_that(
  "Validation of email", 
  {
    TheData <- BasicData
    test_field <- "email_test"
    invalid_value <- "invalid-email"
      
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of letters_only", 
  {
    TheData <- BasicData
    test_field <- "letters_only_test"
    invalid_value <- "123"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of phone", 
  {
    TheData <- BasicData
    test_field <- "phone_test"
    invalid_value <- "not a phone number"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of text_test", 
  {
    TheData <- BasicData
    test_field <- "text_test"
    invalid_value <- "There actually isn't an invalid value for this one"
      
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                    rcon = rcon))
  }
)

test_that(
  "Validation of zipcode", 
  {
    TheData <- BasicData
    test_field <- "zipcode_test"
    invalid_value <- "not a zipcode"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of date_dmy", 
  {
    TheData <- BasicData
    test_field <- "date_dmy_test"
    invalid_value <- "not a date"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]] <- as.character(TheData[[test_field]])
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of date_mdy", 
  {
    TheData <- BasicData
    test_field <- "date_mdy_test"
    invalid_value <- "not a date"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]] <- as.character(TheData[[test_field]])
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of date_ymd", 
  {
    TheData <- BasicData
    test_field <- "date_ymd_test"
    invalid_value <- "not a date"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))

    TheData[[test_field]] <- as.character(TheData[[test_field]])    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of datetime_dmy_hm", 
  {
    TheData <- BasicData
    test_field <- "datetime_dmy_hm_test"
    invalid_value <- "not a date"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))

    TheData[[test_field]] <- as.character(TheData[[test_field]])    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of datetime_mdy_hm", 
  {
    TheData <- BasicData
    test_field <- "datetime_mdy_hm_test"
    invalid_value <- "not a date"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]] <- as.character(TheData[[test_field]])
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of datetime_ymd_hm", 
  {
    TheData <- BasicData
    test_field <- "datetime_ymd_hm_test"
    invalid_value <- "not a date"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))

    TheData[[test_field]] <- as.character(TheData[[test_field]])    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of datetime_dmy_hms", 
  {
    TheData <- BasicData
    test_field <- "datetime_dmy_hms_test"
    invalid_value <- "not a date"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]] <- as.character(TheData[[test_field]])    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of datetime_mdy_hms", 
  {
    TheData <- BasicData
    test_field <- "datetime_mdy_hms_test"
    invalid_value <- "not a date"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]] <- as.character(TheData[[test_field]])
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of datetime_ymd_hms", 
  {
    TheData <- BasicData
    test_field <- "datetime_ymd_hms_test"
    invalid_value <- "not a data"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]] <- as.character(TheData[[test_field]])    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of time_hhmm", 
  {
    TheData <- BasicData
    test_field <- "time_hhmm_test"
    invalid_value <- "not a time"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    
    TheData[[test_field]] <- as.character(TheData[[test_field]])
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of time_mmss", 
  {
    TheData <- BasicData
    test_field <- "time_mmss_test"
    invalid_value <- "not a time"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]] <- as.character(TheData[[test_field]])
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of integer", 
  {
    TheData <- BasicData
    test_field <- "integer_test"
    invalid_value <- "1.2"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of number_1dp_comma", 
  {
    TheData <- BasicData
    test_field <- "number_1dp_comma_test"
    invalid_value <- "invalid value"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                    rcon = rcon))
    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of number_1dp", 
  {
    TheData <- BasicData
    test_field <- "number_1dp_test"
    invalid_value <- "not a number"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of number_2dp_comma", 
  {
    TheData <- BasicData
    test_field <- "number_2dp_comma_test"
    invalid_value <- "not a number"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of number_2dp", 
  {
    TheData <- BasicData
    test_field <- "number_2dp_test"
    invalid_value <- "not a number"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of number", 
  {
    TheData <- BasicData
    test_field <- "number_test"
    invalid_value <- "not a number"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of slider", 
  {
    TheData <- BasicData
    test_field <- "slider_no_label_test"
    invalid_value <- "not a number"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of checkbox", 
  {
    TheData <- BasicData
    test_field <- "checkbox_test___x"
    invalid_value <- "not a checked value"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]] <- as.character(TheData[[test_field]])
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
  }
)

test_that(
  "Validation of dropdown", 
  {
    TheData <- BasicData
    test_field <- "dropdown_test"
    invalid_value <- "wrong value"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]] <- as.character(TheData[[test_field]])
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of radio", 
  {
    TheData <- BasicData
    test_field <- "radio_test"
    invalid_value <- "wrong value"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]] <- as.character(TheData[[test_field]])
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of truefalse", 
  {
    TheData <- BasicData
    test_field <- "truefalse_test"
    invalid_value <- "bad value"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of yesno", 
  {
    TheData <- BasicData
    test_field <- "yesno_test"
    invalid_value <- "bad value"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]] <- as.character(TheData[[test_field]])
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "Validation of notes", 
  {
    TheData <- BasicData
    test_field <- "notes_test"
    invalid_value <- "We can't actually make an invalid value here"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                    rcon = rcon))
  }
)

test_that(
  "Validation of calculated", 
  {
    TheData <- BasicData
    test_field <- "calc_addition"
    invalid_value <- "some text"
    
    expect_silent(castForImport(data = TheData[c("record_id", test_field)], 
                                  rcon = rcon))
    
    TheData[[test_field]][1] <- invalid_value
    
    ImportData <- 
      expect_warning(castForImport(data = TheData[c("record_id", test_field)], 
                                     rcon = rcon), 
                     "Some records failed validation")
    
    expect_true(attr(ImportData, "invalid")$value == invalid_value)
    
    expect_true(is.na(ImportData[[test_field]][1]))
  }
)

test_that(
  "only requested fields are recast", 
  {
    Records <- data.frame(checkbox_test___x = c("0", "", "", "0"), 
                          checkbox_test___y = c("y", "y", "", "y"))
    
    ForImport <- castForImport(Records, 
                               rcon, 
                               fields = "checkbox_test___x",
                               cast = list(checkbox = castCheckForImport(checked = "0")))
    expect_equal(ForImport$checkbox_test___x, 
                 c(1, 0, 0, 1))
    expect_equal(ForImport$checkbox_test___y, 
                 c("y", "y", "", "y"))
  }
)


#####################################################################
# Argument Validation                                            ####

test_that(
  "castForImport Validation", 
  {
    testthat::local_reproducible_output(width = 200)
    expect_error(castForImport(data = "not a data frame", 
                                 rcon = rcon), 
                 "'data': Must be of type 'data.frame'")
    
    expect_error(castForImport(data = BasicData, 
                                 rcon = "not a redcap connection"), 
                 "'rcon': Must inherit from class 'redcapConnection'")
    
    expect_error(castForImport(data = BasicData, 
                                 rcon = rcon, 
                                 fields = TRUE))
    
    expect_error(castForImport(data = BasicData, 
                                 rcon = rcon, 
                                 fields = 100))
    
    expect_error(castForImport(data = BasicData, 
                                 rcon = rcon, 
                                 fields = "not a field name"))
    
    expect_error(castForImport(data = BasicData, 
                                 rcon = rcon, 
                                 na = "not a list"), 
                 "'na': Must be of type 'list'")
    
    expect_error(castForImport(data = BasicData, 
                                 rcon = rcon, 
                                 na = list('unnamed list')), 
                 "'na': Must have names")
    
    expect_error(castForImport(data = BasicData, 
                                 rcon = rcon, 
                                 validation = "not a list"), 
                 "'validation': Must be of type 'list'")
    
    expect_error(castForImport(data = BasicData, 
                                 rcon = rcon, 
                                 validation = list('unnamed list')), 
                 "'validation': Must have names")
    
    expect_error(castForImport(data = BasicData, 
                                 rcon = rcon, 
                                 cast = "not a list"), 
                 "'cast': Must be of type 'list'")
    
    expect_error(castForImport(data = BasicData, 
                                 rcon = rcon, 
                                 cast = list('unnamed list')), 
                 "'cast': Must have names")
  }
)

context("widerRepeated Functionality")

Raw <- exportRecordsTyped(rcon, 
                          cast = raw_cast)
MetaData <- rcon$metadata()

rcon_off <- offlineConnection(meta_data = MetaData, 
                              records = Raw)

test_that("widerRepeated works to widen forms with repeating instrument",{
  x <- exportRecordsTyped(rcon, forms="repeating_instrument")
  wider_x <- widerRepeated(x, rcon)
  expect_no_error(widerRepeated(x, rcon))
  expect_false(identical(x, wider_x))
  
})

test_that("widerRepeated returns original data without repeating instrument",{
  x <- exportRecordsTyped(rcon, forms="randomization")
  wider_x <- widerRepeated(x, rcon)
  expect_no_error(widerRepeated(x, rcon))
  expect_equal(x, wider_x)

})

test_that("widerRepeated returns an error if the redcap_repeat_instrument column has more than one unique instrument",{
  x <- exportRecordsTyped(rcon, forms="repeating_instrument")
  # add row with different redcap_repeat_instrument
  new_row <- data.frame(
    record_id = "1",
    redcap_event_name = "event_1_arm_1",
    redcap_repeat_instrument = "randomization",
    redcap_repeat_instance = '3',
    repeat_question_1 = "Good",
    repeat_datetime = "2023-05-10 09:00:00",
    repeating_instrument_complete = "Incomplete"
  )
  x <- rbind(x, new_row)
  expect_error(widerRepeated(x, rcon))
  
})

test_that("widerRepeated works if column attribute is invalid with no repeating instrument",{
  expect_warning(
    x <- exportRecordsTyped(
      rcon,
      fields="prereq_number",
      validation=list(number=valRx("^5$|^-100$"))),
    "failed validation")
  
  wider_x <- widerRepeated(x, rcon)
  expect_no_error(widerRepeated(x, rcon))
  expect_equal(x, wider_x)
  
})

test_that("widerRepeated works if column attribute is invalid with repeating instrument",{
  f <- function(x, field_name, coding) {g <- valRx("^Good$"); if(field_name == 'repeat_question_1') g(x) else rep(TRUE, length(x))}
  
  expect_warning(
    x <- exportRecordsTyped(
      rcon,
      fields="repeat_question_1",
      validation=list(text=f)),
    "failed validation")
  
  wider_x <- widerRepeated(x, rcon)
  expect_no_error(widerRepeated(x, rcon))
  expect_false(identical(x, wider_x))
  
})

###################################################################
# Test cases offline

test_that("widerRepeated works to widen forms with repeating instrument offline",{
  x <- exportRecordsTyped(rcon_off, forms="repeating_instrument")
  wider_x <- widerRepeated(x, rcon_off)
  expect_no_error(widerRepeated(x, rcon_off))
  expect_false(identical(x, wider_x))
  
})

test_that("widerRepeated returns original data without repeating instrument offline",{
  x <- exportRecordsTyped(rcon_off, forms="randomization")
  wider_x <- widerRepeated(x, rcon_off)
  expect_no_error(widerRepeated(x, rcon_off))
  expect_equal(x, wider_x)
  
})

test_that("widerRepeated returns an error if the redcap_repeat_instrument column has more than one unique instrument offline",{
  x <- exportRecordsTyped(rcon_off, forms="repeating_instrument")
  # add row with different redcap_repeat_instrument
  new_row <- data.frame(
    record_id = "1",
    redcap_event_name = "event_1_arm_1",
    redcap_repeat_instrument = "randomization",
    redcap_repeat_instance = '3',
    repeat_question_1 = "Good",
    repeat_datetime = "2023-05-10 09:00:00",
    repeating_instrument_complete = "Incomplete"
  )
  x <- rbind(x, new_row)
  expect_error(widerRepeated(x, rcon_off))
  
})

test_that("widerRepeated works if column attribute is invalid with no repeating instrument offline",{
  expect_warning(
    x <- exportRecordsTyped(
      rcon_off,
      fields="prereq_number",
      validation=list(number=valRx("^5$|^-100$"))),
    "failed validation")
  
  wider_x <- widerRepeated(x, rcon_off)
  expect_no_error(widerRepeated(x, rcon_off))
  expect_equal(x, wider_x)
  
})

context("widerRepeated Functionality")

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
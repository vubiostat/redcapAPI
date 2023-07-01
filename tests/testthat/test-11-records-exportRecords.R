context("exportRecords")

test_that(
  "exportRecords with defaults", 
  {
    Rec <- exportRecords(rcon)
    expect_false("redcap_data_access_group" %in% names(Rec))
    expect_character(Rec$date_dmy)
    expect_character(Rec$dropdown_test)
    expect_true(all(Rec$dropwdown_test %in% c(NA, 1, 2, 3)))
  }
)

test_that(
  "exportRecords returns only requested records", 
  {
    Rec <- exportRecords(rcon, records = 10:11)
    
    expect_true(all(Rec$record_id %in% 10:11))
  }
)

test_that(
  "exportRecords returns only requested fields", 
  {
    Rec <- exportRecords(rcon, 
                         fields = c("record_id", "dropdown_test", "date_dmy"))
    expect_equal(names(Rec),
                 c("record_id", "redcap_event_name", "redcap_repeat_instrument", 
                   "redcap_repeat_instance", "date_dmy", "dropdown_test"))
  }
)

test_that(
  "exportRecords returns only fields in requested forms", 
  {
    Rec <- exportRecords(rcon, 
                         forms = "randomization")
    expect_equal(names(Rec), 
                 c("treatment", "randomization_complete"))
  }
)

test_that(
  "exportRecords returns only requested events", 
  {
    Rec <- exportRecords(rcon, 
                         events = "event_1_arm_1")
    expect_true(all(Rec$redcap_event_name %in% "event_1_arm_1"))
  }
)

test_that(
  "exportRecords returns labelled values", 
  {
    Rec <- exportRecords(rcon, 
                         raw_or_label = "label")
    expect_true(all(Rec$dropwdown_test %in% c(NA, 1, 2, 3)))
  }
)

test_that(
  "exportRecords returns labelled variable names", 
  {
    Rec <- exportRecords(rcon, 
                         raw_or_label_headers = "label")
    expect_equal(names(Rec)[1:2], 
                 c("Record.ID", "Event.Name"))
  }
)

test_that(
  "exportRecords returns labelled checkbox values", 
  {
    Rec <- exportRecords(rcon, 
                         raw_or_label = "label", 
                         export_checkbox_label = TRUE)
    expect_true(all(Rec$checkbox_test___x %in% c(NA, "Guitar")))
    expect_true(all(Rec$checkbox_test___y %in% c(NA, "Ukulele")))
  }
)

test_that(
  "exportRecords responds to batch_size", 
  {
    Rec <- exportRecords(rcon, 
                         batch_size = 5)
    expect_data_frame(Rec)
  }
)

test_that(
  "exportRecords response to csv_delimiter", 
  {
    Rec <- exportRecords(rcon, 
                         csv_delimiter = ";")
    expect_data_frame(Rec)
  }
)

# FIXME: Add tests for export_dags and export_survey_fields
context("Export Typed Records Offline Functionality")

load(file.path(test_path("testdata"), "RedcapProject_test_redcapAPI.Rdata"))

suppressWarnings({
  roff <- offlineConnection(meta_data = RedcapProject_test_redcapAPI$meta_data, 
                            arms = RedcapProject_test_redcapAPI$arms, 
                            events = RedcapProject_test_redcapAPI$events, 
                            mapping = RedcapProject_test_redcapAPI$mappings, 
                            project_info = RedcapProject_test_redcapAPI$project_information, 
                            repeat_instrument = RedcapProject_test_redcapAPI$repeating_instruments, 
                            records = RedcapProject_test_redcapAPI$records)
})

test_that(
  "Data are returned", 
  {
    expect_data_frame(exportRecordsTyped(roff))
  }
)

test_that(
  "Only requested fields are returned", 
  {
    these_fields <- c("record_id", "dropdown_test", "integer_test")
    Rec <- exportRecordsTyped(roff, 
                              fields = these_fields)
    expect_true(all(c(these_fields, REDCAP_SYSTEM_FIELDS) %in% names(Rec)))
  }
)

test_that(
  "Drop fields are removed", 
  {
    Rec <- exportRecordsTyped(roff, 
                              drop_fields = "number_test")
    expect_false("number_test" %in% names(Rec))
  }
)

test_that(
  "Only fields in requested forms are returned", 
  {
    Rec <- exportRecordsTyped(roff, 
                              forms = "multiple_choice")
    expect_equal(names(Rec), 
                 c("record_id", "redcap_event_name", "redcap_data_access_group", 
                   "redcap_repeat_instrument", "redcap_repeat_instance", "checkbox_test___x", 
                   "checkbox_test___y", "checkbox_test___z", "dropdown_test", "radio_test", 
                   "truefalse_test", "yesno_test", "multiple_choice_complete"))
  }
)

test_that(
  "Only fields in requested events are returned", 
  {
    Rec <- exportRecordsTyped(roff, 
                              events = "event_2_arm_2")
    expect_data_frame(Rec, 
                      nrows = 0)
  }
)

context("exportRecords")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that("records can be exported",{
  expect_message(rec <- exportRecords(rcon))
  expect_gte(length(rec), 1)
})

#!! for tests regarding variable conversions, see test-fieldToVar.R


# Tests for fields, forms, records ----------------------------------
test_that(
  "Records returned for designated fields", 
  {
    fields_to_get <- c("record_id", 
                       "date_ymd", 
                       "prereq_number")
    Records <- exportRecords(rcon, 
                             fields = fields_to_get)
    expect_subset(fields_to_get, 
                  choices = names(Records))
  }
)

test_that(
  "fields in the designated forms are returned", 
  {
    forms_to_get <- c("fieldtovar_datetimes", 
                      "branching_logic")
    Records <- exportRecords(rcon, 
                             forms = forms_to_get)
    expect_false("treatment" %in% names(Records))
  }
)

test_that(
  "fields in the drop= arg are not returned", 
  {
    forms_to_drop <- c("treatment")
    Records <- exportRecords(rcon, 
                             drop = forms_to_drop)
    expect_false("treatment" %in% names(Records))
  }
)

test_that(
  "records returned only for designated records", 
  {
    records_to_get <- 1:3
    Records <- exportRecords(rcon, 
                             records = records_to_get)
    expect_true(all(Records$record_id %in% records_to_get))
  }
)

test_that(
  "Data returned only for designated event", 
  {
    Records <- exportRecords(rcon, 
                             events = "event_1_arm_1")
    expect_true(all(Records$redcap_event_name %in% "event_1_arm_1"))
  }
)

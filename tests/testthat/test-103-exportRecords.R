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


# Argument validation tests -----------------------------------------

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords("not an rcon"), 
                 "no applicable method for 'exportRecords'")
  }
)

test_that(
  "Return an error if factors is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               factors = c(TRUE, FALSE)), 
                 "'factors'[:] Must have length 1")
    expect_error(exportRecords(rcon, factors = "TRUE"), 
                 "Variable 'factors'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if fields is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               fields = 1:2), 
                 "'fields'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if forms is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               fields = 1:2), 
                 "'fields'[:] Must be of type 'character'")
  }
)


test_that(
  "Return an error if records is not numeric or character", 
  {
    local_reproducible_output(width = 200)
    WithCharacter <- exportRecords(rcon, records = c("1", "2"))
    WithNumeric <- exportRecords(rcon, records = c(1, 2))
    
    expect_identical(WithCharacter, 
                     WithNumeric)
    expect_error(exportRecords(rcon, records = TRUE), 
                 "'records'[:] Must be of type 'character'")
  } 
)

test_that(
  "Return an error if events is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, events = 1), 
                 "'events'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if labels is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               labels = c(TRUE, FALSE)), 
                 "'labels'[:] Must have length 1")
    expect_error(exportRecords(rcon, 
                               labels = "TRUE"), 
                 "'labels'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if dates is not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               dates = c(TRUE, FALSE)), 
                 "'dates'[:] Must have length 1")
    expect_error(exportRecords(rcon, 
                               dates = "TRUE"), 
                 "'dates'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if survey is not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               survey = c(TRUE, FALSE)), 
                 "'survey'[:] Must have length 1")
    expect_error(exportRecords(rcon, 
                               survey = "TRUE"), 
                 "'survey'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if dag is not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               dag = c(TRUE, FALSE)), 
                 "'dag'[:] Must have length 1")
    expect_error(exportRecords(rcon, 
                               dag = "TRUE"), 
                 "'dag'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if checkboxLabels is not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               checkboxLabels = c(TRUE, FALSE)), 
                 "'checkboxLabels'[:] Must have length 1")
    expect_error(exportRecords(rcon, 
                               checkboxLabels = "TRUE"), 
                 "'checkboxLabels'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if colClasses is not a named vector", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(exportRecords(rcon, 
                               colClasses = list("character", "numeric", "character")))
    
    expect_error(exportRecords(rcon, 
                               colClasses = c("character", "numeric", "numeric")))
  }
)

test_that(
  "Return an error if batch.size is not integerish(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               batch.size = c(-1, 100)), 
                 "'batch.size'[:] Must have length 1")
    expect_error(exportRecords(rcon, 
                               batch.size = "-1"), 
                 "'batch.size'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error if form_complete_auto is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               form_complete_auto = c(TRUE, FALSE)), 
                 "'form_complete_auto'[:] Must have length 1")
    expect_error(exportRecords(rcon, 
                               form_complete_auto = "TRUE"), 
                 "'form_complete_auto'[:] Must be of type 'logical'")
  }
)

context("exportRecordsTyped")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that("records can be exported",{
  rec <- exportRecordsTyped(rcon)
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
    Records <- exportRecordsTyped(rcon,
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
    Records <- exportRecordsTyped(rcon, forms = forms_to_get)
    expect_false("treatment" %in% names(Records))
  }
)

test_that(
  "fields in the drop_fields= arg are not returned",
  {
    forms_to_drop <- c("treatment")
    Records <- exportRecordsTyped(rcon, drop_fields = forms_to_drop)
    expect_false("treatment" %in% names(Records))
  }
)

test_that(
  "records returned only for designated records",
  {
    records_to_get <- 1:3
    Records <- exportRecordsTyped(rcon,
                             records = records_to_get)
    expect_true(all(Records$record_id %in% records_to_get))
  }
)

test_that(
  "Data returned only for designated event",
  {
    Records <- exportRecordsTyped(rcon, events = "event_1_arm_1")
    expect_true(all(Records$redcap_event_name %in% "event_1_arm_1"))
  }
)

# Argument validation tests -----------------------------------------

test_that(
  "Return an error if rcon is not a redcapConnection",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped("not an rcon"),
                 "no applicable method for 'exportRecordsTyped'")
  }
)

test_that(
  "Return an error if fields is not character",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                               fields = 1:2),
                 "'fields'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if forms is not character",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                               fields = 1:2),
                 "'fields'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if records is not numeric or character",
  {
    local_reproducible_output(width = 200)
    WithCharacter <- exportRecordsTyped(rcon, records = c("1", "2"))
    WithNumeric <- exportRecordsTyped(rcon, records = c(1, 2))

    expect_identical(WithCharacter,
                     WithNumeric)
    expect_error(exportRecordsTyped(rcon, records = TRUE),
                 "'records'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if events is not character",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon, events = 1),
                 "'events'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if survey is not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                               survey = c(TRUE, FALSE)),
                 "'survey'[:] Must have length 1")
    expect_error(exportRecordsTyped(rcon,
                               survey = "TRUE"),
                 "'survey'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if dag is not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                               dag = c(TRUE, FALSE)),
                 "'dag'[:] Must have length 1")
    expect_error(exportRecordsTyped(rcon,
                               dag = "TRUE"),
                 "'dag'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if batch.size is not integerish(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                                    batch_size = c(-1, 100)),
                 "'batch_size': Must have length <= 1, but has length 2.")
    expect_error(exportRecordsTyped(rcon,
                                    batch_size = "-1"),
                 "'batch_size'[:] Must be of type 'integerish'")
  }
)

context("exportRecordsTyped redcapApiConnection")

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
  "Return an error if drop_fields is not character",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                                    drop_fields = 1:2),
                 "'drop_fields'[:] Must be of type 'character'")
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
  "Return an error if events is not character",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon, events = 1),
                 "'events'[:] Must be of type 'character'")
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
  "Return an error if date_begin is not POSIXct",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                                    date_begin=TRUE),
                 "'date_begin'[:] Must be of type 'POSIXct'")
  }
)

test_that(
  "Return an error if date_end is not POSIXct",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                                    date_end=FALSE),
                 "'date_end'[:] Must be of type 'POSIXct'")
  }
)


test_that(
  "Return an error if batch_size is not integerish(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                                    batch_size = c(-1, 100)),
                 "'batch_size'[:] Must have length <= 1, but has length 2.")
    expect_error(exportRecordsTyped(rcon,
                                    batch_size = "-1"),
                 "'batch_size'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error if csv_delimiter is not an allowed character",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                                    csv_delimiter="*"),
                 "'csv_delimiter'[:] Must be element of set")
    expect_error(exportRecordsTyped(rcon,
                                    csv_delimiter=",,"),
                 "'csv_delimiter'[:] Must be element of set")
  }
)

test_that(
  "Return an error if config is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                                    config=1:3),
                 "'config'[:] Must be of type 'list'")
    expect_error(exportRecordsTyped(rcon,
                                    config=list("a")),
                 "'config'[:] Must have names")
  }
)

test_that(
  "Return an error if api_param is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                                    api_param=1:3),
                 "'api_param'[:] Must be of type 'list'")
    expect_error(exportRecordsTyped(rcon,
                                    api_param=list("a")),
                 "'api_param'[:] Must have names")
  }
)

test_that(
  "Return an error if na is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                                    na=1:3),
                 "'na'[:] Must be of type 'list'")
    expect_error(exportRecordsTyped(rcon,
                                    na=list("a")),
                 "'na'[:] Must have names")
  }
)

test_that(
  "Return an error if validation is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                                    validation=1:3),
                 "'validation'[:] Must be of type 'list'")
    expect_error(exportRecordsTyped(rcon,
                                    validation=list("a")),
                 "'validation'[:] Must have names")
  }
)

test_that(
  "Return an error if cast is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                                    cast=1:3),
                 "'cast'[:] Must be of type 'list'")
    expect_error(exportRecordsTyped(rcon,
                                    cast=list("a")),
                 "'cast'[:] Must have names")
  }
)

test_that(
  "Return an error if assignment is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,
                                    assignment=1:3),
                 "'assignment'[:] Must be of type 'list'")
    expect_error(exportRecordsTyped(rcon,
                                    assignment=list("a")),
                 "'assignment'[:] Must have names")
  }
)


test_that(
  "Return an error if mChoice is not logical(1) or 'labelled' or 'coded'",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon, mChoice = c(TRUE, FALSE)),
                 "'mChoice'[:] .* Must have length 1")
    expect_error(exportRecordsTyped(rcon, mChoice = "TRUE"),
                 "'mChoice'[:] One of the following must apply")
  }
)

# Meta-data validations versus parameters
test_that(
  "Return an error if fields specified doesn't exist",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,fields='doesntexist'),
                 "'fields'[:] Must be a subset of")
  }
)

test_that(
  "Return an error if drop_fields specified doesn't exist",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,drop_fields='doesntexist'),
                 "'drop_fields'[:] Must be a subset of")
  }
)

test_that(
  "Return an error if forms specified doeesn't exist",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,forms='doesntexist'),
                 "'forms'[:] Must be a subset of")
  }
)

test_that(
  "Return an error if events specified doeesn't exist",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon,events='doesntexist'),
                 "'events'[:] Must be a subset of")
  }
)

 ###################################################################
# Test attribute assignments versus defaults
rec <- exportRecordsTyped(rcon)
test_that(
  "HTML and Unicode is stripped by default",
  expect_equal(attr(rec$date_dmy, "label"), "Date (D-M-Y)")
)

test_that(
  "Units are assigned from annotations",
  expect_equal(attr(rec$date_dmy, "units"), "time")
)
rm(rec)

 ###################################################################
# NA Detection
test_that(
  "NA can be override for user definitions",
  {
    rec <- exportRecordsTyped(rcon, na=list(date_=function(x, ...) is.na(x) | x=="" | x == "2023-02-24"))
    expect_true(is.na(rec$date_dmy[1]))
  }
)

 ###################################################################
# Validation
test_that(
  "Custom validation works",
  {
    rec  <- exportRecordsTyped(rcon, fields="prereq_number", cast=raw_cast)
    recV <- expect_warning(
              exportRecordsTyped(
                rcon,
                fields="prereq_number",
                validation=list(number=valRx("^5$|^-100$"))),
              "failed validation")
    inv <- attr(recV, "invalid")
    expect_true(!is.null(inv))
    expect_equal(unique(inv$value), "1")
    sapply(c(14, 15, 18, 19), function(i) expect_true(!i %in% inv$row))
    sapply(1:13, function(i) expect_true(i %in% inv$row))
  }
)

 ###################################################################
# Casting
test_that(
  "Dates can be cast using as.Date",
  {
    rec <- exportRecordsTyped(rcon, cast=list(date_=as.Date))
    expect_class(rec$date_dmy, "Date")
  }
)

test_that(
  "Raw cast works",
  { 
    rec <- exportRecordsTyped(rcon, cast=raw_cast)
    expect_equal(rec$date_dmy[1], "2023-02-24")
    expect_class(rec$date_dmy[1], "character")
  }
)

context("exportRecordsTyped redcapOfflineConnection")


Raw <- exportRecordsTyped(rcon, 
                          cast = raw_cast)
MetaData <- rcon$metadata()

rcon_off <- offlineConnection(meta_data = MetaData, 
                              records = Raw)

test_that("records can be exported",{
  rec <- exportRecordsTyped(rcon_off)
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
    Records <- exportRecordsTyped(rcon_off,
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
    Records <- exportRecordsTyped(rcon_off, forms = forms_to_get)
    expect_false("treatment" %in% names(Records))
  }
)

test_that(
  "fields in the drop_fields= arg are not returned",
  {
    forms_to_drop <- c("treatment")
    Records <- exportRecordsTyped(rcon_off, drop_fields = forms_to_drop)
    expect_false("treatment" %in% names(Records))
  }
)

test_that(
  "records returned only for designated records",
  {
    records_to_get <- 1:3
    Records <- exportRecordsTyped(rcon_off,
                             records = records_to_get)
    expect_true(all(Records$record_id %in% records_to_get))
  }
)

test_that(
  "Data returned only for designated event",
  {
    Records <- exportRecordsTyped(rcon_off, events = "event_1_arm_1")
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
    expect_error(exportRecordsTyped(rcon_off,
                               fields = 1:2),
                 "'fields'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if drop_fields is not character",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon_off,
                                    drop_fields = 1:2),
                 "'drop_fields'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if forms is not character",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon_off,
                               fields = 1:2),
                 "'fields'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if events is not character",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon_off, events = 1),
                 "'events'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if records is not numeric or character",
  {
    local_reproducible_output(width = 200)
    WithCharacter <- exportRecordsTyped(rcon_off, records = c("1", "2"))
    WithNumeric <- exportRecordsTyped(rcon_off, records = c(1, 2))

    expect_identical(WithCharacter,
                     WithNumeric)
    expect_error(exportRecordsTyped(rcon_off, records = TRUE),
                 "'records'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if na is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon_off,
                                    na=1:3),
                 "'na'[:] Must be of type 'list'")
    expect_error(exportRecordsTyped(rcon_off,
                                    na=list("a")),
                 "'na'[:] Must have names")
  }
)

test_that(
  "Return an error if validation is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon_off,
                                    validation=1:3),
                 "'validation'[:] Must be of type 'list'")
    expect_error(exportRecordsTyped(rcon_off,
                                    validation=list("a")),
                 "'validation'[:] Must have names")
  }
)

test_that(
  "Return an error if cast is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon_off,
                                    cast=1:3),
                 "'cast'[:] Must be of type 'list'")
    expect_error(exportRecordsTyped(rcon_off,
                                    cast=list("a")),
                 "'cast'[:] Must have names")
  }
)

test_that(
  "Return an error if assignment is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon_off,
                                    assignment=1:3),
                 "'assignment'[:] Must be of type 'list'")
    expect_error(exportRecordsTyped(rcon_off,
                                    assignment=list("a")),
                 "'assignment'[:] Must have names")
  }
)

# Meta-data validations versus parameters
test_that(
  "Return an error if fields specified doesn't exist",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon_off,fields='doesntexist'),
                 "'fields'[:] Must be a subset of")
  }
)

test_that(
  "Return an error if drop_fields specified doesn't exist",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon_off,drop_fields='doesntexist'),
                 "'drop_fields'[:] Must be a subset of")
  }
)

test_that(
  "Return an error if forms specified doeesn't exist",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(rcon_off,forms='doesntexist'),
                 "'forms'[:] Must be a subset of")
  }
)

 ###################################################################
# Test attribute assignments versus defaults
rec <- exportRecordsTyped(rcon_off)
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
    rec <- exportRecordsTyped(rcon_off, na=list(date_=function(x, ...) is.na(x) | x=="" | x == "2023-02-24"))
    expect_true(is.na(rec$date_dmy[1]))
  }
)

 ###################################################################
# Validation
test_that(
  "Custom validation works",
  {
    # This is about the only test that is different from the redcapApiConnection test. 
    # It turns out the API only returns records with at least one non-missing value
    # but the data download includes all the records. So the data sets are 
    # a different size between the two methods. Curious.
    rec  <- exportRecordsTyped(rcon_off, fields="prereq_number", cast=raw_cast)
    recV <- expect_warning(
              exportRecordsTyped(
                rcon_off,
                fields="prereq_number",
                validation=list(number=valRx("^5$|^-100$"))),
              "failed validation")
    inv <- attr(recV, "invalid")
    expect_true(!is.null(inv))
    expect_equal(unique(inv$value), "1")
    sapply(c(23, 24, 25, 26), function(i) expect_true(!i %in% inv$row))
    sapply(1:13, function(i) expect_true(i %in% inv$row))
  }
)

 ###################################################################
# Casting
test_that(
  "Dates can be cast using as.Date",
  {
    rec <- exportRecordsTyped(rcon_off, cast=list(date_=as.Date))
    expect_class(rec$date_dmy, "Date")
  }
)

test_that(
  "Raw cast works",
  { 
    rec <- exportRecordsTyped(rcon_off, cast=raw_cast)
    expect_equal(rec$date_dmy[1], "2023-02-24")
    expect_class(rec$date_dmy[1], "character")
  }
)

#####################################################################
# Avoid error on System Fields (Issue #102)                      ####

test_that(
  "Including system fields in 'fields' doesn't produce an error", 
  {
    # FIXME: This test would be better run on a project that has
    #        repeating instruments and events, and possibly DAGs
    # Four use cases from #102
    
    # 1. User requests no fields (fields = NULL) return all fields
    #    This is covered in other tests.
    
    # 2. User requests only actual fields (no system fields in 'fields')
    #    Return actual fields + system fields
    
    Rec <- exportRecordsTyped(rcon_off, 
                              fields = "record_id")
    expect_true("redcap_event_name" %in% names(Rec))
    
    # 3. User requests actual fields + system fields. 
    #    Return only the requested fields
    # FIXME: This test would be better if it had more system fields 
    #        available in the project
    
    Rec <- exportRecordsTyped(rcon_off, 
                              fields = c("record_id", "redcap_event_name"))
    expect_true(all(c("record_id", "redcap_event_name") %in% names(Rec)))
    # 4. User requests only system fields
    #    Return only system fields
    
    Rec <- exportRecordsTyped(rcon_off, 
                              fields = c("redcap_event_name"))
    expect_true(names(Rec) == "redcap_event_name")
  }
)

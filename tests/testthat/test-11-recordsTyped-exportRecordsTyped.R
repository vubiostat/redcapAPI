context("exportRecordsTyped Functionality")

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

####################################################################
# mChoice specific tests

require(Hmisc)

test_that(
  "records can be exported with Hmisc attached",
  {
    skip_if(!requireNamespace("Hmisc", quietly=TRUE), 
            "Hmisc is required to test mChoice export")
    rec <- exportRecordsTyped(rcon)
    expect_gte(length(rec), 1)
  }
)

test_that(
  "mChoice type conversion for checkbox with Hmisc attached",
  {
    skip_if(!requireNamespace("Hmisc", quietly=TRUE), 
            "Hmisc is required to test mChoice type conversion")
    
    rec <- exportRecordsTyped(rcon)
    expect_class(rec$prereq_checkbox, "mChoice")
    
    rec <- exportRecordsTyped(rcon, mChoice=TRUE)
    expect_class(rec$prereq_checkbox, "mChoice")
    
    rec <- exportRecordsTyped(rcon, mChoice=FALSE)
    expect_false("prereq_checkbox" %in% names(rec))
    
    rec <- exportRecordsTyped(rcon, mChoice="labelled")
    expect_class(rec$prereq_checkbox, "mChoice")
    
    rec <- exportRecordsTyped(rcon, mChoice="coded")
    expect_class(rec$prereq_checkbox, "mChoice")
    
  }
)

test_that(
  "mChoice works for coded",
  {
    rec <- exportRecordsTyped(rcon, mChoice="coded")
    expect_equal(levels(rec$prereq_checkbox), c("1", "2", "ABC", "4"))
  }
)

test_that(
  "mChoice works for TRUE and defaults to coded",
  {
    rec <- exportRecordsTyped(rcon, mChoice=TRUE)
    expect_equal(levels(rec$prereq_checkbox), c("1", "2", "ABC", "4"))
  }
)

test_that(
  "mChoice works for NULL and defaults to coded",
  {
    rec <- exportRecordsTyped(rcon)
    expect_equal(levels(rec$prereq_checkbox), c("1", "2", "ABC", "4"))
  }
)


test_that(
  "mChoice works for labelled",
  {
    rec <- exportRecordsTyped(rcon, mChoice="labelled")
    expect_equal(levels(rec$prereq_checkbox),
                 c("Checkbox1", "Checkbox2", "CheckboxABC", "Do not use in branching logic"))
  }
)

# Without Hmisc Tests
detach("package:Hmisc", unload=TRUE)

test_that(
  "mChoice with no Hmisc warns user if requested",
  {
    expect_warning(rec <- exportRecordsTyped(rcon, mChoice=TRUE), "Hmisc")
    expect_false("prereq_checkbox" %in% names(rec))
    
    expect_warning(rec <- exportRecordsTyped(rcon, mChoice="coded"), "Hmisc")
    expect_false("prereq_checkbox" %in% names(rec))
    
    expect_warning(rec <- exportRecordsTyped(rcon, mChoice="labelled"), "Hmisc")
    expect_false("prereq_checkbox" %in% names(rec))
  }
)
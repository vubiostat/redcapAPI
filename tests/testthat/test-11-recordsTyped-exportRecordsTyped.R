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
    expect_data_frame(inv)
    expect_equal(names(inv), 
                 c("row", "field_name", "field_type", "value"))
    sapply(c(14, 15, 18, 19), function(i) expect_true(!i %in% inv$row))
    sapply(1:13, function(i) expect_true(i %in% inv$row))
    

    # Validation report where the id_field is present has the correct number of columns
    recV <- expect_warning(
      exportRecordsTyped(
        rcon,
        fields=c("record_id", "prereq_number"),
        validation=list(number=valRx("^5$|^-100$"))),
      "failed validation")
    inv <- attr(recV, "invalid")
    expect_true(!is.null(inv))
    expect_equal(unique(inv$value), "1")
    expect_data_frame(inv)
    expect_equal(names(inv), 
                 c("row", "record_id", "field_name", "field_type", "value"))
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

#####################################################################
# Export calculated fields

test_that(
  "Calculated fields are exported", 
  {
    expect_data_frame(
      exportRecordsTyped(rcon, 
                         fields = c("left_operand", "right_operand", 
                                    "calc_addition", "calc_squared")), 
      ncols = 4
    )
  }
)

#####################################################################
# Export for a single record

test_that(
  "Export succeeds for a single record", 
  {
    expect_data_frame(
      exportRecordsTyped(rcon, 
                         records = "1"), 
      nrows = 1
    )
    
    expect_data_frame(
      exportRecordsTyped(rcon, 
                         records = "1", 
                         forms = "branching_logic"), 
      nrows = 1
    )
    
    expect_data_frame(
      exportRecordsTyped(rcon, 
                         records = "1", 
                         fields = "record_id", "date_dmy"), 
      nrows = 1
    )
  }
)

#####################################################################
# Yes/No fields are cast properly

test_that(
  "Yes/No fields are labelled correctly", 
  {
    Rec <- exportRecordsTyped(rcon, fields = "prereq_yesno")
    
    expect_true(all(Rec$prereq_yesno %in% c("Yes", "No", NA)))
  }
)

#####################################################################
# Avoid error on System Fields (Issue #102)                      ####

test_that(
  "Including system fields in 'fields' doesn't produce an error", 
  {
    expect_no_error(exportRecordsTyped(rcon, 
                                       fields = REDCAP_SYSTEM_FIELDS))
  }
)

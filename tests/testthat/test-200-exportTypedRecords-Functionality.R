context("Export Typed Records Functionality")

# Testing in this file focuses on casting values to their appropriate types.
# The project is loaded with arms and events out of necessity.  
# Data Access Groups, Surveys, and Repeating Instruments are not included
# yet, and functionality around events will be tested later.
#
# Subsequent files will deal with each of those specifics

load(file.path(test_path("testdata"),
               "test_redcapAPI_MetaData.Rdata"))
load(file.path(test_path("testdata"), 
               "test_redcapAPI_Data.Rdata"))
load(file.path(test_path("testdata"), 
               "test_redcapAPI_Arms.Rdata"))
load(file.path(test_path("testdata"), 
               "test_redcapAPI_Events.Rdata"))

rcon$flush_all()

forms <- c("record_id", "text_fields", "dates_and_times", "numbers", 
           "slider_fields", "multiple_choice", 
           "files_notes_descriptions", "calculated_fields")
MetaData <- test_redcapAPI_MetaData[test_redcapAPI_MetaData$form_name %in% forms, ]

importMetaData(rcon, 
               MetaData)
importArms(rcon, 
           arms_data = test_redcapAPI_Arms)
importEvents(rcon, 
             event_data = test_redcapAPI_Events)

importProjectInformation(rcon, 
                         data.frame(is_longitudinal = 1, 
                                    record_autonumbering_enabled = 0))

Mappings <- data.frame(arm_num = rep(1, length(forms)), 
                       unique_event_name = rep("event_1_arm_1", length(forms)), 
                       form = forms)
importMappings(rcon, 
               data = Mappings)


ImportData <- test_redcapAPI_Data[names(test_redcapAPI_Data) %in% MetaData$field_name]
ImportData <- ImportData[!is.na(ImportData$email_test), ]
ImportData <- castForImport(ImportData, rcon)


importRecords(rcon, 
              ImportData)

#####################################################################
# Functional Testing                                             ####
test_that("records can be exported",{
  rec <- exportRecordsTyped(rcon)
  expect_gte(length(rec), 1)
})

#####################################################################
# Tests for fields, forms, records                               ####
test_that(
  "Records returned for designated fields",
  {
    fields_to_get <- c("record_id",
                       "date_ymd_test",
                       "number_test")
    Records <- exportRecordsTyped(rcon,
                                  fields = fields_to_get)
    expect_subset(fields_to_get,
                  choices = names(Records))
  }
)

test_that(
  "fields in the designated forms are returned",
  {
    forms_to_get <- c("numbers",
                      "slider_fields")
    Records <- exportRecordsTyped(rcon, forms = forms_to_get)
    expect_false("email_test" %in% names(Records))
  }
)

test_that(
  "fields in the drop_fields= arg are not returned",
  {
    forms_to_drop <- c("email_test")
    Records <- exportRecordsTyped(rcon, drop_fields = forms_to_drop)
    expect_false("email_test" %in% names(Records))
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



#####################################################################
# Test attribute assignments versus defaults                     ####

MetaData$field_annotation[w] <- "units={\"time\"}"
importMetaData(rcon, MetaData)
rec <- exportRecordsTyped(rcon)

test_that(
  "HTML and Unicode is stripped by default",
  expect_equal(attr(rec$date_dmy_test, "label"), "Date field DMY")
)

test_that(
  "Units are assigned from annotations",
  {
    w <- which(MetaData$field_name == "date_dmy_test")
    expect_equal(attr(rec$date_dmy_test, "units"), "time")
  }
)
rm(rec)

#####################################################################
# NA Detection                                                   ####
test_that(
  "NA can be override for user definitions",
  {
    importRecords(rcon, 
                  data.frame(record_id = 1, 
                             date_dmy_test = "2023-02-24"))
    rec <- exportRecordsTyped(rcon, na=list(date_=function(x, ...) is.na(x) | x=="" | x == "2023-02-24"))
    expect_true(is.na(rec$date_dmy_test[1]))
  }
)

#####################################################################
# Validation                                                     ####
test_that(
  "Custom validation works",
  {
    importRecords(rcon, 
                  data = data.frame(record_id = c(1, 2, 3, 4, 5), 
                                    number_test = c(5, -100, 7, 7, 7)))
    rec  <- exportRecordsTyped(rcon, 
                               fields="number_test", 
                               records = 1:5,
                               cast=raw_cast)
    recV <- expect_warning(
      exportRecordsTyped(
        rcon,
        records = 1:5,
        fields="number_test",
        validation=list(number=valRx("^5$|^-100$"))),
      "failed validation")
    inv <- attr(recV, "invalid")
    expect_true(!is.null(inv))
    expect_equal(unique(inv$value), "7")
    expect_data_frame(inv)
    expect_equal(names(inv), 
                 c("row", "record_id", "field_name", "field_type", "value"))
    sapply(c(1:2), function(i) expect_true(!i %in% inv$row))
    sapply(3:5, function(i) expect_true(i %in% inv$row))
    
    
    # Validation report where the id_field is present has the correct number of columns
    recV <- expect_warning(
      exportRecordsTyped(
        rcon,
        records = 1:5,
        fields=c("record_id", "number_test"),
        validation=list(number=valRx("^5$|^-100$"))),
      "failed validation")
    inv <- attr(recV, "invalid")
    expect_true(!is.null(inv))
    expect_equal(unique(inv$value), "7")
    expect_data_frame(inv)
    expect_equal(names(inv), 
                 c("row", "record_id", "field_name", "field_type", "value"))
  }
)

#####################################################################
# Casting                                                        ####
test_that(
  "Dates can be cast using as.Date",
  {
    rec <- exportRecordsTyped(rcon, cast=list(date_=as.Date))
    expect_class(rec$date_dmy_test, "Date")
  }
)

test_that(
  "Raw cast works",
  { 
    importRecords(rcon, 
                  data.frame(record_id = 1, 
                             date_dmy_test = "2023-02-24"))
    rec <- exportRecordsTyped(rcon, cast=raw_cast)
    expect_equal(rec$date_dmy[1], "2023-02-24")
    expect_class(rec$date_dmy[1], "character")
  }
)

#####################################################################
# Export calculated fields                                       ####

test_that(
  "Calculated fields are exported", 
  {
    expect_data_frame(
      exportRecordsTyped(rcon, 
                         fields = c("left_operand", "right_operand", 
                                    "calc_addition", "calc_squared")), 
      ncols = 6
    )
  }
)

#####################################################################
# Export for a single record                                     ###f

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
                         forms = "numbers"), 
      nrows = 1
    )
    
    expect_data_frame(
      exportRecordsTyped(rcon, 
                         records = "1", 
                         fields = "record_id", "date_dmy_test"), 
      nrows = 1
    )
  }
)

#####################################################################
# Yes/No fields are cast properly                                ####

test_that(
  "Yes/No fields are labelled correctly", 
  {
    Rec <- exportRecordsTyped(rcon, fields = "yesno_test")
    
    expect_true(all(Rec$yesno_test %in% c("Yes", "No", NA)))
  }
)


#####################################################################
# Return error messages from the API                             ####

test_that(
  "Return error messages from the API",
  {
    # we are adding a non existent field through api_param to force an error from 
    # the API. 
    expect_error(exportRecordsTyped(rcon, 
                                    api_param = list(fields = "this_wont_work_abc123")), 
                 "The following values in the parameter \"fields\" are not valid")
  }
)

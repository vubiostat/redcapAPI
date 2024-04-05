context("Export Typed Records Functionality")

# Testing in this file focuses on casting values to their appropriate types.
# The project is loaded with arms and events out of necessity.  
# Data Access Groups, Surveys, and Repeating Instruments are not included
# yet, and functionality around events will be tested later.
#
# Subsequent files will deal with each of those specifics

purgeProject(rcon, records=TRUE)
load(file.path(test_path("testdata"),
               "test_redcapAPI_MetaData.Rdata"))
load(file.path(test_path("testdata"), 
               "test_redcapAPI_Data.Rdata"))
load(file.path(test_path("testdata"), 
               "test_redcapAPI_Arms.Rdata"))
load(file.path(test_path("testdata"), 
               "test_redcapAPI_Events.Rdata"))

forms <- c("record_id", "text_fields", "dates_and_times", "numbers", 
           "slider_fields", "multiple_choice", 
           "files_notes_descriptions", "calculated_fields")
MetaData <- test_redcapAPI_MetaData[test_redcapAPI_MetaData$form_name %in% forms, ]

importMetaData(rcon, MetaData)
importArms(rcon,     test_redcapAPI_Arms)
importEvents(rcon,   test_redcapAPI_Events)
importProjectInformation(rcon, 
                         data.frame(is_longitudinal = 1, 
                                    record_autonumbering_enabled = 0))

Mappings <- data.frame(arm_num = rep(1, length(forms)), 
                       unique_event_name = rep("event_1_arm_1", length(forms)), 
                       form = forms)
importMappings(rcon, Mappings)


ImportData <- test_redcapAPI_Data[names(test_redcapAPI_Data) %in% MetaData$field_name]
ImportData <- ImportData[!is.na(ImportData$email_test), ]
# castForImport only needed until 3.0.0
ImportData <- castForImport(ImportData, 
                            rcon, 
                            validation = list(bioportal = valSkip),
                            cast = list(number_1dp = as.numeric, 
                                        number_2dp = as.numeric, 
                                        number_1dp_comma_decimal = as.numeric, 
                                        number_2dp_comma_decimal = as.numeric, 
                                        bioportal = as.character))


importRecords(rcon, ImportData)


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

w <- which(MetaData$field_name == "date_dmy_test")
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
    expect_equal(attr(rec$date_dmy_test, "units"), "time")
  }
)

test_that(
  "Checkbox fields labels include the choice", 
  {
    expect_equal(attr(rec$checkbox_test___x, "label"), 
                 "Checkbox Example (choice=Guitar)")
    expect_equal(attr(rec$checkbox_test___y, "label"), 
                 "Checkbox Example (choice=Ukulele)")
    expect_equal(attr(rec$checkbox_test___z, "label"), 
                 "Checkbox Example (choice=Mandolin)")
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
                 c("row", "record_id", "field_name", "form_name", "field_type", "event_id", "value", "link_to_form"))
    sapply(c(1:2), function(i) expect_true(!i %in% inv$row))
    sapply(3:5, function(i) expect_true(i %in% inv$row))
    
    # Get the invalid attribute using reviewInvalidRecords
    
    inv <- reviewInvalidRecords(recV)
    expect_true(!is.null(inv))
    expect_equal(unique(inv$value), "7")
    expect_data_frame(inv)
    expect_equal(names(inv), 
                 c("row", "record_id", "field_name", "form_name", "field_type", "event_id", "value", "link_to_form"))
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
                 c("row", "record_id", "field_name", "form_name", "field_type", 
                   "event_id", "value", "link_to_form"))
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
    fields <- c("left_operand", "right_operand","calc_addition", "calc_squared")
    expect_data_frame(x <- exportRecordsTyped(rcon, fields = fields), 
                      min.cols = 6)
    expect_subset(fields, names(x))
  }
)

#####################################################################
# Export for a single record                                     ####

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

#####################################################################
# Handle Zero Coded Check Values - Issue 199                     ####

test_that(
  "Casting Zero-coded check values works correctly", 
  {
    local_reproducible_output(width = 200)
    # Create a zero coded check field
    NewMetaData <- test_redcapAPI_MetaData
    NewMetaData <- NewMetaData[NewMetaData$field_name %in% c("record_id", 
                                                             "checkbox_test"), ]
    NewMetaData$field_name[2] <- "checkbox_zero"
    NewMetaData$field_label[2] <- "Zero Coded Checkbox Example"
    NewMetaData$select_choices_or_calculations[2] <- "0, Zero | 1, One | 2, Two"
    
    importMetaData(rcon, NewMetaData)
    
    importRecords(rcon, 
                  data = data.frame(record_id = 1:4,
                                    checkbox_zero___0 = c(0, 1, 0, 1)))
    
    # Under default casting -----------------------------------------
    expect_warning(DefaultRecord <- exportRecordsTyped(rcon, 
                                                       fields = "checkbox_zero___0", 
                                                       records = 1:4, 
                                                       assignment = list()), 
                   "Zero-coded check fields found")
    
    expect_equal(DefaultRecord$checkbox_zero___0, 
                 factor(c(0, 1, 0, 1), 
                        levels = 0:1, 
                        labels = c("Unchecked", "Checked")))
    
    # Recast to Coded
    expect_warning(
      Recast1 <- recastRecords(DefaultRecord, 
                               rcon, 
                               fields = "checkbox_zero___0",
                               cast = list(checkbox = castCheckCode)), 
      "Zero-coded check fields found")
    expect_equal(Recast1$checkbox_zero___0, 
                 factor(c(0, 1, 0, 1), 
                        levels = 0:1, 
                        labels = c("", "0")))
    
    # Recast to Label
    expect_warning(
      Recast2 <- recastRecords(DefaultRecord, 
                               rcon, 
                               fields = "checkbox_zero___0",
                               cast = list(checkbox = castCheckLabel)))
    expect_equal(Recast2$checkbox_zero___0, 
                 factor(c(0, 1, 0, 1), 
                        levels = 0:1, 
                        labels = c("", "Zero")))
    # Recast to Raw
    expect_warning(
      Recast3 <- recastRecords(DefaultRecord, 
                               rcon, 
                               fields = "checkbox_zero___0",
                               cast = list(checkbox = castRaw)))
    expect_equal(Recast3$checkbox_zero___0, 
                 c(0, 1, 0, 1))
    
    
    
    # Under Cast to coding ------------------------------------------
    expect_warning(
      CodeRecord <- exportRecordsTyped(rcon, 
                                       fields = "checkbox_zero___0", 
                                       records = 1:4, 
                                       assignment = list(), 
                                       cast = list(checkbox = castCheckCode)))
    
    expect_equal(CodeRecord$checkbox_zero___0, 
                 factor(c(0, 1, 0, 1), 
                        levels = 0:1, 
                        labels = c("", "0")))
    
    # Recast to Checked - This scenario doesn't cast correctly
    expect_warning(Recast1 <- recastRecords(CodeRecord, 
                                            rcon, 
                                            fields = "checkbox_zero___0",
                                            cast = list(checkbox = castChecked)), 
                   "Zero-coded check field .+ may not have been cast correctly")
    expect_equal(Recast1$checkbox_zero___0, 
                 factor(c(0, 0, 0, 0), 
                        levels = 0:1, 
                        labels = c("Unchecked", "Checked")))
    
    # Recast to Label - This scenario doesn't cast correctly
    expect_warning(
      Recast2 <- recastRecords(CodeRecord, 
                             rcon, 
                             fields = "checkbox_zero___0",
                             cast = list(checkbox = castCheckLabel)))
    expect_equal(Recast2$checkbox_zero___0, 
                 factor(c(0, 0, 0, 0), 
                        levels = 0:1, 
                        labels = c("", "Zero")))
    # Recast to Raw - This scenario doesn't cast correctly
    expect_warning(
      Recast3 <- recastRecords(CodeRecord, 
                             rcon, 
                             fields = "checkbox_zero___0",
                             cast = list(checkbox = castRaw)))
    expect_equal(Recast3$checkbox_zero___0, 
                 c(0, 0, 0, 0)) 
    
    
    
    
    # Under Cast to label -------------------------------------------
    expect_warning(
      LabelRecord <- exportRecordsTyped(rcon, 
                                     fields = "checkbox_zero___0", 
                                     records = 1:4, 
                                     assignment = list(), 
                                     cast = list(checkbox = castCheckLabel)))
    
    expect_equal(LabelRecord$checkbox_zero___0, 
                 factor(c(0, 1, 0, 1), 
                        levels = 0:1, 
                        labels = c("", "Zero")))
    
    # Recast to Checked
    expect_warning(
      Recast1 <- recastRecords(LabelRecord, 
                             rcon, 
                             fields = "checkbox_zero___0",
                             cast = list(checkbox = castChecked)))
    expect_equal(Recast1$checkbox_zero___0, 
                 factor(c(0, 1, 0, 1), 
                        levels = 0:1, 
                        labels = c("Unchecked", "Checked")))
    
    # Recast to Coded
    expect_warning(
      Recast2 <- recastRecords(LabelRecord, 
                             rcon, 
                             fields = "checkbox_zero___0",
                             cast = list(checkbox = castCheckCode)))
    expect_equal(Recast2$checkbox_zero___0, 
                 factor(c(0, 1, 0, 1), 
                        levels = 0:1, 
                        labels = c("", "0")))
    # Recast to Raw
    expect_warning(
      Recast3 <- recastRecords(LabelRecord, 
                             rcon, 
                             fields = "checkbox_zero___0",
                             cast = list(checkbox = castRaw)))
    expect_equal(Recast3$checkbox_zero___0, 
                 c(0, 1, 0, 1))
    
    
    
    # Restore the meta data for further testing ---------------------
    importMetaData(rcon, MetaData)
  }
)

#####################################################################
# Casting to Characters (no factors)                             ####

test_that(
  "Casting to character with no factors", 
  {
    Rec <- exportRecordsTyped(rcon, 
                              cast = default_cast_no_factor)
    expect_data_frame(Rec)
    
    expect_false(any(vapply(Rec, is.factor, logical(1))))
  }
)

test_that(
  "Casting to character with no factors (using alternate list)", 
  {
    Rec <- exportRecordsTyped(rcon, 
                              cast = default_cast_character)
    expect_data_frame(Rec)
    
    expect_false(any(vapply(Rec, is.factor, logical(1))))
  }
)


#####################################################################
# Batching with multiple events                                  ####

test_that(
  "Batching behaves correctly when records have data in multiple events", 
  {
    importEvents(rcon, 
                 data = data.frame(event_name = "Event 2", 
                                   arm_num = 1))
    
    OrigMapping <- rcon$mapping()
    
    NewMapping <- OrigMapping
    NewMapping$unique_event_name <- "event_2_arm_1"
    NewMapping <- cbind(OrigMapping, NewMapping)
    
    importMappings(rcon, data = NewMapping)
    
    importRecords(rcon, 
                  data = data.frame(record_id = 1, 
                                    redcap_event_name = "event_2_arm_1"))
    
    Unbatched <- exportRecordsTyped(rcon)
    
    Batched <- exportRecordsTyped(rcon, batch_size = 1)
    
    expect_true(identical(Unbatched, Batched))
    
    deleteEvents(rcon, 
                 events = "event_2_arm_1")
    
    importMappings(rcon, 
                   OrigMapping)
  }
)

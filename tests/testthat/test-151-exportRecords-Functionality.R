context("Export Records Basic Functionality")

#####################################################################
# Set up project for testing                                     ####
purgeProject(rcon, purge_all = TRUE)

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
# Actual Testing :)                                              ####

test_that(
  "Get data using defaults", 
  {
    Rec <- exportRecords(rcon)
    
    expect_data_frame(Rec)
    expect_gte(length(Rec), 1)
  }
)

#####################################################################
# Tests for fields, forms, records                               ####
test_that(
  "Records returned for designated fields",
  {
    fields_to_get <- c("record_id",
                       "date_ymd_test",
                       "number_test")
    Records <- exportRecords(rcon,
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
    Records <- exportRecords(rcon, forms = forms_to_get)
    expect_false("email_test" %in% names(Records))
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

#####################################################################
# Export calculated fields                                       ####

test_that(
  "Calculated fields are exported", 
  {
    expect_data_frame(
      exportRecords(rcon, 
                    fields = c("left_operand", "right_operand", 
                               "calc_addition", "calc_squared")), 
      ncols = 4
    )
  }
)


#####################################################################
# Export for a single record                                     ####

test_that(
  "Export succeeds for a single record", 
  {
    expect_data_frame(
      exportRecords(rcon, 
                         records = "1"), 
      nrows = 1
    )
    
    expect_data_frame(
      exportRecords(rcon, 
                         records = "1", 
                         forms = "numbers"), 
      nrows = 1
    )
    
    expect_data_frame(
      exportRecords(rcon, 
                         records = "1", 
                         fields = c("record_id", "date_dmy_test")), 
      nrows = 1
    )
  }
)

#####################################################################
# Return error messages from the API                             ####

test_that(
  "Return error messages from the API",
  {
    # we are adding a non existent field through api_param to force an error from 
    # the API. 
    expect_error(exportRecords(rcon, 
                               api_param = list(fields = "this_wont_work_abc123")), 
                 "The following values in the parameter \"fields\" are not valid")
  }
)


#####################################################################
# Test raw_or_label behavior                                     ####

test_that(
  "raw_or_label returns intended output", 
  {
    Rec <- exportRecords(rcon, 
                         fields = c("dropdown_test", "yesno_test"),
                         raw_or_label = "label")
    expect_true(all(Rec$dropdown_test %in% c("Lavender", "Blue", "Green")))
    expect_true(all(Rec$yesno_test %in% c("No", "Yes")))
  }
)

test_that(
  "raw_or_label_headers changes variable names", 
  {
    RecRaw <- exportRecords(rcon, 
                            raw_or_label_headers = "raw")
    RecLab <- exportRecords(rcon, 
                            raw_or_label_headers = "label")
    
    expect_false(any(names(RecRaw) == names(RecLab)))
  }
)
 
test_that(
  "export_checkbox_label behaviors", 
  {
    RawFalse <- exportRecords(rcon, 
                         fields = c("checkbox_test"),
                         raw_or_label = "raw",
                         export_checkbox_label = FALSE)
    
    RawTrue <- exportRecords(rcon, 
                             fields = c("checkbox_test"),
                             raw_or_label = "raw",
                             export_checkbox_label = TRUE)
    
    expect_true(all(RawFalse$checkbox_test___x == RawTrue$checkbox_test___x))
    
    LabelFalse <- exportRecords(rcon, 
                                fields = c("checkbox_test"),
                                raw_or_label = "label",
                                export_checkbox_label = FALSE)
    
    expect_true(all(LabelFalse$checkbox_test___x %in% c("Checked", "Unchecked")))
    
    LabelTrue <- exportRecords(rcon, 
                               fields = c("checkbox_test"),
                               raw_or_label = "label",
                               export_checkbox_label = TRUE)
    
    expect_true(all(LabelTrue$checkbox_test___x %in% c(NA, "Guitar")))
  }
)   

#####################################################################
# test batch_size                                                ####

test_that(
  "Export Records with Batch Size", 
  {
    expect_silent(exportRecords(rcon, batch_size = 10))
  }
)

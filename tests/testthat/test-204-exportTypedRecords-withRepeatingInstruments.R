context("Export Typed Records with Repeating Events and Instruments Functionality")

#####################################################################
# Prepare data with Repeating Instruments                        ####

load(file.path(test_path("testdata"),
               "test_redcapAPI_MetaData.Rdata"))
load(file.path(test_path("testdata"), 
               "test_redcapAPI_Data.Rdata"))

importMetaData(rcon, 
               test_redcapAPI_MetaData)

forms <- rcon$instruments()$instrument_name
Mappings <- data.frame(arm_num = rep(1, length(forms)), 
                       unique_event_name = rep("event_1_arm_1", length(forms)), 
                       form = forms)
importMappings(rcon, 
               data = Mappings)

RepeatInst <- data.frame(event_name = "event_1_arm_1", 
                         form_name = "repeating_instrument")

importRepeatingInstrumentsEvents(rcon, 
                                 data = RepeatInst)

# castForImport only needed until 3.0.0
ImportData <- castForImport(test_redcapAPI_Data,
                            rcon,
                            validation = list(bioportal = valSkip),
                            cast = list(number_1dp = as.numeric,
                                        number_2dp = as.numeric,
                                        number_1dp_comma_decimal = as.numeric,
                                        number_2dp_comma_decimal = as.numeric, 
                                        bioportal = as.character))

importRecords(rcon, ImportData)

#######################################################################
# Export Records with Repeating Instruments                        ####

test_that(
  "Exported data has repeating instruments", 
  {
    Rec <- exportRecordsTyped(rcon)
    expect_true(any(!is.na(Rec$redcap_repeat_instrument)))
    expect_true(any(!is.na(Rec$redcap_repeat_instrument)))
  }
)

test_that(
  "Repeating Instrument name can be labelled or raw", 
  {
    Rec <- exportRecordsTyped(rcon, 
                              forms = "repeating_instrument")
    
    expect_equal(levels(Rec$redcap_repeat_instrument), 
                 rcon$instruments()$instrument_label)
    
    
    
    Rec <- exportRecordsTyped(rcon, 
                              forms = "repeating_instrument", 
                              cast = raw_cast)
    
    expect_equal(unique(Rec$redcap_repeat_instrument), 
                 "repeating_instrument")
  }
)

#####################################################################
# Avoid error on System Fields (Issue #102)                      ####
# These tests are placed here because by this point we have all
# of the system fields included.
test_that(
  "Including system fields in 'fields' doesn't produce an error",
  {
    # Four use cases from #102

    # 1. User requests no fields (fields = NULL) return all fields
    #    This is covered in other tests.

    # 2. User requests only actual fields (no system fields in 'fields')
    #    Return actual fields + system fields

    Rec <- exportRecordsTyped(rcon,
                              fields = "record_id",
                              dag=TRUE)
    expect_equal(names(Rec),
                 c("record_id", "redcap_event_name", "redcap_repeat_instrument", 
                  "redcap_repeat_instance", "redcap_data_access_group"))

    # 3. User requests actual fields + system fields.
    #    Return only the requested fields
    # FIXME: This test would be better if it had more system fields
    #        available in the project

    Rec <- exportRecordsTyped(rcon,
                              fields = c("record_id", "redcap_event_name", 
                                         "number_test"))
    expect_equal(names(Rec), 
                 c("record_id", "redcap_event_name", "number_test"))

    # 4. User requests only system fields
    #    Return only system fields

    Rec <- exportRecordsTyped(rcon,
                              fields = c("redcap_event_name", 
                                         "redcap_data_access_group"))
    expect_equal(names(Rec),
                 c("redcap_event_name", 
                   "redcap_data_access_group"))
  }
)


#####################################################################
# Always include ID fields                                       ####

test_that(
  "ID fields are included on all calls",
  {
    minimum_field <- c("record_id",
                       "redcap_event_name",
                       "redcap_repeat_instrument",
                       "redcap_repeat_instance",
                       "redcap_data_access_group")

    # ID field and system fields when just the ID field is requested

    Rec <- exportRecordsTyped(rcon,
                              fields = "record_id",
                              dag=TRUE)
    expect_equal(names(Rec),
                 minimum_field)

    # ID field and system fields when a single form is requested

    Rec <- exportRecordsTyped(rcon,
                              forms = c("randomization"),
                              dag=TRUE)
    expect_true(all(minimum_field %in% names(Rec)))

    # Now let's make a secondary unique field
    NewInfo <- data.frame(secondary_unique_field = "text_test")
    importProjectInformation(rcon, NewInfo)

    Rec <- exportRecordsTyped(rcon,
                              forms = c("randomization"),
                              dag=TRUE)
    expect_true(all(c(minimum_field, "text_test") %in% names(Rec)))

    NewInfo <- data.frame(secondary_unique_field = "",
                          surveys_enabled = 0)
    importProjectInformation(rcon, NewInfo)
  }
)

#####################################################################
# Behavior of filter_empty_rows                                  ####

test_that(
  "filter_empty_rows filters some rows",
  {
    expect_true(
      nrow(exportRecordsTyped(rcon, fields="treatment", filter_empty_rows=TRUE)) <
        nrow(exportRecordsTyped(rcon, fields="treatment", filter_empty_rows=FALSE)) 
    )
  }
)

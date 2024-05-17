context("Repeating Instruments and Events Methods Functionality")

#####################################################################
# Set up arms and events for testing importMappings data         ####

load(file.path(test_path("testdata", 
                         "test_redcapAPI_MetaData.Rdata")))
load(file.path(test_path("testdata", 
                         "test_redcapAPI_Data.Rdata")))

Arms <- data.frame(arm_num = 1:2, 
                   name = c("Arm 1", "Arm 2"), 
                   stringsAsFactors = FALSE)

Events <- data.frame(event_name = c("event_1", 
                                    "event_2", 
                                    "event_1"), 
                     arm_num = c(1, 1, 2), 
                     unique_event_name = c("event_1_arm_1", 
                                           "event_2_arm_2", 
                                           "event_1_arm_2"), 
                     stringsAsFactors = FALSE)


importMetaData(rcon, 
               test_redcapAPI_MetaData[test_redcapAPI_MetaData$form_name %in% 
                                         c("record_id", "repeating_instrument"), ])

importArms(rcon, 
           data = Arms)

importEvents(rcon, 
             data = Events)

importMappings(rcon, 
               data = data.frame(arm_num = 1, 
                                 unique_event_name = "event_1_arm_1", 
                                 form = "repeating_instrument"))

importProjectInformation(rcon, 
                         data = data.frame(is_longitudinal = 1, 
                                           has_repeating_instruments_or_events = 1))

test_that(
  "Import and Export Repeating Instrument Settings", 
  {
    # First we add a repeating instrument
    Repeat <- data.frame(event_name = "event_1_arm_1", 
                         form_name = "repeating_instrument")
    
    n_imported <- importRepeatingInstrumentsEvents(rcon, 
                                                   data = Repeat)
    expect_equal(n_imported, "1")
    
    # Now let's check that we get the setting on the export
    expect_data_frame(exportRepeatingInstrumentsEvents(rcon), 
                      ncols = 3)
    expect_equal(Repeat$event_name, 
                 rcon$repeatInstrumentEvent()$event_name)
    expect_equal(Repeat$form_name, 
                 rcon$repeatInstrumentEvent()$form_name)
    
    local_reproducible_output(width = 200)

    expect_error(exportRepeatingInstrumentsEvents(rcon, 
                                                  config = list(1)), 
                 "'config': Must have names")
    expect_error(exportRepeatingInstrumentsEvents(rcon, 
                                                  config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportRepeatingInstrumentsEvents(rcon, 
                                                  api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportRepeatingInstrumentsEvents(rcon, 
                                                  api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
    
    # Now let's back out the changes
    n_imported <- importRepeatingInstrumentsEvents(rcon, 
                                                   REDCAP_REPEAT_INSTRUMENT_STRUCTURE)
    expect_equal(n_imported, "0")
  
    expect_data_frame(exportRepeatingInstrumentsEvents(rcon), 
                      nrows = 0)
  }
)

purgeProject(rcon, 
             arms = TRUE, 
             events = TRUE)

test_that(
  "Functionality on a test with no events or arms", 
  {
    expect_equal(rcon$projectInformation()$has_repeating_instruments_or_events, 
                 0)
    
    expect_data_frame(exportRepeatingInstrumentsEvents(rcon), 
                      ncols = 3, 
                      nrows = 0)
    
    NewRepeat <- data.frame(event_name = c("event_1_arm_1"), 
                            form_name = NA_character_, 
                            custom_form_label = NA_character_)
    
    expect_error(importRepeatingInstrumentsEvents(rcon, 
                                                    NewRepeat),
                   "event 'event_1_arm_1' is not available in project")
    
    expect_data_frame(exportRepeatingInstrumentsEvents(rcon), 
                      ncols = 3, 
                      nrows = 0)
  }
)

#####################################################################
# Clean up                                                       ####

purgeProject(rcon, purge_all = TRUE)

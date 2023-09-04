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

rcon$refresh_arms()
rcon$refresh_events()



test_that(
  "Import and Export Repeating Instrument Settings", 
  {
    # First we add a repeating instrument
    Repeat <- data.frame(event_name = "event_1_arm_1", 
                         form_name = "repeating_instrument")
    
    expect_message(importRepeatingInstrumentsEvents(rcon, 
                                                    data = Repeat), 
                   "Rows imported: 1")
    
    rcon$refresh_projectInformation()
    rcon$refresh_repeatInstrumentEvent()
    
    # Now let's check that we get the setting on the export
    expect_data_frame(exportRepeatingInstrumentsEvents(rcon), 
                      ncols = 3)
    expect_equal(Repeat$event_name, 
                 rcon$repeatInstrumentEvent()$event_name)
    expect_equal(Repeat$form_name, 
                 rcon$repeatInstrumentEvent()$form_name)
    
    
    # Now let's back out the changes
    expect_message(importRepeatingInstrumentsEvents(rcon, 
                                                    REDCAP_REPEAT_INSTRUMENT_STRUCTURE), 
                   "Rows imported: 0")
  
    expect_data_frame(exportRepeatingInstrumentsEvents(rcon), 
                      nrows = 0)
  }
)


purgeProject(rcon, 
             arms = TRUE, 
             events = TRUE)
rcon$refresh_arms()
rcon$refresh_events()

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

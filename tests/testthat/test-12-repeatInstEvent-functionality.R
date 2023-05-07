context("Export and Import Repeating Instruments and Events Functionality")

load(test_path("testdata", "RedcapProject_RepeatingInstrument.Rdata"))

purgeProject(rcon, purge_all = TRUE)
rcon$flush_all() # Clear the cache.
restoreProject(RedcapProject_RepeatingInstrument, rcon)
rcon$flush_all()

#####################################################################
# exportRepeatingInstrumentsEvents                               ####

test_that(
  "Test exportRepeatingInstrumentsEvents functionality",
  {
    local_reproducible_output(width = 200)
    
    expect_data_frame(exportRepeatingInstrumentsEvents(rcon), 
                      ncols = 3)
  }
)

#####################################################################
# importRepeatingInstrumentsEvents                               ####

test_that(
  "Test importRepeatingInstrumentEvents",
  {
    local_reproducible_output(width = 200)
    
    Current <- rcon$repeatInstrumentEvent()
    
    ToUpload <- Current[1, ]
    
    expect_message(importRepeatingInstrumentsEvents(rcon, 
                                                    ToUpload), 
                   sprintf("Rows imported: %s", nrow(ToUpload)))
    
    expect_equal(nrow(rcon$repeatInstrumentEvent()), 
                 nrow(ToUpload))
    
    # import original without refreshing
    expect_message(importRepeatingInstrumentsEvents(rcon, 
                                                    Current, 
                                                    refresh = FALSE), 
                   sprintf("Rows imported: %s", nrow(Current)))
    
    expect_equal(nrow(rcon$repeatInstrumentEvent()), 
                 nrow(ToUpload))
    
    rcon$refresh_repeatInstrumentEvent()
    
    expect_equal(nrow(rcon$repeatInstrumentEvent()), 
                 nrow(Current))
  }
)

#####################################################################
# Other functionality tests                                      ####

test_that(
  "Tests for projects configured with repeating instruments", 
  {
    rcon$flush_all()
    Repeat <- rcon$repeatInstrumentEvent()
    
    # Confirms that importing a subset only retains the subset
    expect_message(importRepeatingInstrumentsEvents(rcon, 
                                                    Repeat[1, ]), 
                   "Rows imported: 1")
    
    expect_data_frame(exportRepeatingInstrumentsEvents(rcon), 
                      ncols = 3, 
                      nrows = 1)
    
    # Restore to the original
    expect_message(importRepeatingInstrumentsEvents(rcon, 
                                                    Repeat), 
                   "Rows imported: 2")
    
    expect_data_frame(exportRepeatingInstrumentsEvents(rcon), 
                      ncols = 3, 
                      nrows = 2)
    
    # Confirm that we can delete settings
    expect_message(importRepeatingInstrumentsEvents(rcon, 
                                                    REDCAP_REPEAT_INSTRUMENT_STRUCTURE), 
                   "Rows imported: 0")
    
    expect_data_frame(exportRepeatingInstrumentsEvents(rcon), 
                      ncols = 3, 
                      nrows = 0)
    
    rcon$refresh_projectInformation()
    
    # With no settings uploaded, the project information should reflect that
    # has_repeating_instruments_or_events is false.
    expect_equal(rcon$projectInformation()$has_repeating_instruments_or_events, 
                 0)
  }
)



purgeProject(rcon, 
             purge_all = TRUE)

load(test_path("testdata", "RedcapProject_BasicData.Rdata"))
rcon$flush_all()
restoreProject(RedcapProject_BasicData, 
               rcon)

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
    
    expect_message(importRepeatingInstrumentsEvents(rcon, 
                                                    NewRepeat),
                    "Rows imported: 1")
    
    expect_data_frame(exportRepeatingInstrumentsEvents(rcon), 
                      ncols = 3, 
                      nrows = 0)
  }
)


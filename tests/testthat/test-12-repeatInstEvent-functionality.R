context("Export and Import Repeating Instruments and Events Functionality")

#####################################################################
# exportRepeatingInstrumentsEvents                               ####

test_that(
  {
    local_reproducible_output(width = 200)
    skip_if(!STRUCTURAL_TEST_READY, 
            "Infrastructure not quite ready for structural tests.")
    
    expect_data_frame(exportRepeatingInstrumentsEvents(rcon), 
                      ncols = 3)
  }
)

#####################################################################
# importRepeatingInstrumentsEvents                               ####

test_that(
  {
    local_reproducible_output(width = 200)
    skip_if(!STRUCTURAL_TEST_READY, 
            "Infrastructure not quite ready for structural tests.")
    
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
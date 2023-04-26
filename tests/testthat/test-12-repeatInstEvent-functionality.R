context("Export and Import Repeating Instruments and Events Functionality")

rcon <- redcapConnection(url = url, token = API_KEY)
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

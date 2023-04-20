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
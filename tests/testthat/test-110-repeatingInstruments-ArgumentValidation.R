context("Repeating Instruments and Events Methods Argument Validation")

#####################################################################
# exportRepeatingInstrumentEvents Argument Validation            ####

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRepeatingInstrumentsEvents("not an rcon"), 
                 "no applicable method for 'exportRepeatingInstrumentsEvents'")
  }
)


#####################################################################
# importRepeatingInstrumentEvents Argument Validation            ####

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRepeatingInstrumentsEvents("not an rcon", 
                                                  data = REDCAP_REPEAT_INSTRUMENT_STRUCTURE), 
                 "no applicable method for 'importRepeatingInstrumentsEvents'")
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)

    expect_error(importRepeatingInstrumentsEvents(rcon, 
                                                  data = REDCAP_REPEAT_INSTRUMENT_STRUCTURE,
                                                  config = list(1)), 
                 "'config': Must have names")
    expect_error(importRepeatingInstrumentsEvents(rcon, 
                                                  data = REDCAP_REPEAT_INSTRUMENT_STRUCTURE,
                                                  config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(importRepeatingInstrumentsEvents(rcon, 
                                                  data = REDCAP_REPEAT_INSTRUMENT_STRUCTURE,
                                                  api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(importRepeatingInstrumentsEvents(rcon, 
                                                  data = REDCAP_REPEAT_INSTRUMENT_STRUCTURE,
                                                  api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

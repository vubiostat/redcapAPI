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

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRepeatingInstrumentsEvents(rcon, 
                                                  error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
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
  "Return an error if refresh is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRepeatingInstrumentsEvents(rcon, 
                                                  data = REDCAP_REPEAT_INSTRUMENT_STRUCTURE,
                                                  refresh = "TRUE"), 
                 "'refresh': Must be of type 'logical'")
    
    expect_error(importRepeatingInstrumentsEvents(rcon, 
                                                  data = REDCAP_REPEAT_INSTRUMENT_STRUCTURE,
                                                  refresh = c(TRUE, FALSE)), 
                 "'refresh': Must have length 1")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRepeatingInstrumentsEvents(rcon, 
                                                  data = REDCAP_REPEAT_INSTRUMENT_STRUCTURE,
                                                  error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
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

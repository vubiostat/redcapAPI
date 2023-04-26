context("Repeated Instruments and Events Argument Validation")

rcon <- redcapConnection(url = url, 
                         token = API_KEY)

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
# exportRepeatingInstrumentEvents Argument Validation            ####


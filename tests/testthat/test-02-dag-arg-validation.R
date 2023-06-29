# FIXME: Add tests after writing functions

#####################################################################
# exportDags                                                     ####

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportDags(rcon = "not an rcon"), 
                 "no applicable method for 'exportDags' applied")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportDags(rcon, 
                            error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportDags(rcon, 
                            config = list(1)), 
                 "'config': Must have names")
    expect_error(exportDags(rcon, 
                            config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportDags(rcon, 
                            api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportDags(rcon, 
                            api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

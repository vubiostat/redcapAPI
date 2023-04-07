context("Export/Import Project Information")

rcon <- redcapConnection(url = url, 
                         token = SANDBOX_KEY)

#####################################################################
# exportProjectInformation argument validation

test_that(
  "Return an error when rcon is not redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportProjectInformation("not an rcon"), 
                 "no applicable method for 'exportProjectInformation'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportProjectInformation(rcon, 
                                          error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportProjectInformation(rcon, 
                                          config = list(1)), 
                 "'config': Must have names")
    expect_error(exportProjectInformation(rcon, 
                                          config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportProjectInformation(rcon, 
                                          api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportProjectInformation(rcon, 
                                          api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)


#####################################################################
# importProjectInformation argument validation

# FIXME: Add tests after writing importProjectInformation
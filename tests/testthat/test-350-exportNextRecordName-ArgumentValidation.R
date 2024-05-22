context("Export Next Record Name Argument Validation")

#####################################################################
# Argument Validation

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportNextRecordName("not an rcon"), 
                 "no applicable method for 'exportNextRecordName'")
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)

    expect_error(exportNextRecordName(rcon, 
                                      config = list(1)), 
                 "'config': Must have names")
    expect_error(exportNextRecordName(rcon, 
                                      config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportNextRecordName(rcon, 
                                      api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportNextRecordName(rcon, 
                                      api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

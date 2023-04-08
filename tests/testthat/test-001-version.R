context("exportVersion.R")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "exportVersion returns a version number", 
  {
    expect_silent(version <- exportVersion(rcon))
    expect_true(grepl("\\d{1,4}[.]\\d{1,4}[.]\\d{1,4}", version))
  }
)

#####################################################################
# Argument Validation

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportVersion("not an rcon"), 
                 "no applicable method for 'exportVersion'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportVersion(rcon, 
                               error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportVersion(rcon, 
                            config = list(1)), 
                 "'config': Must have names")
    expect_error(exportVersion(rcon, 
                            config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportVersion(rcon, 
                            api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportVersion(rcon, 
                            api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

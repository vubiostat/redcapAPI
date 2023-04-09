context("Export / Import Meta Data Argument Validation")

rcon <- redcapConnection(url = url, token = API_KEY)

#####################################################################
# exportMetaData

test_that(
  "Return an error when rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportMetaData("not an rcon"), 
                 "no applicable method for 'exportMetaData'")
  }
)

test_that(
  "Return an error when fields is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportMetaData(rcon, fields = 1:3), 
                 "'fields'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error when forms is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportMetaData(rcon, forms = 1:3), 
                 "'forms'[:] Must be of type 'character'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportMetaData(rcon, 
                                error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportMetaData(rcon, 
                                config = list(1)), 
                 "'config': Must have names")
    expect_error(exportMetaData(rcon, 
                                config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportMetaData(rcon, 
                                api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportMetaData(rcon, 
                                api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# importMetaData

# FIXME: Add tests when the method has been written.
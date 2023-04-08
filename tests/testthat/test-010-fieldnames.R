context("exportFieldsNames.R")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Return a data frame when called with defaults",
  {
    expect_data_frame(
      exportFieldNames(rcon),
      ncols = 3
    )
  }
)

test_that(
  "Return a data frame with only requested field names", 
  {
    expect_data_frame(
      exportFieldNames(rcon, 
                       fields = c("record_id")), 
      ncols = 3, 
      nrows = 1
    )
    
    expect_data_frame(
      exportFieldNames(rcon, 
                       fields = c("prereq_radio")), 
      ncols = 3, 
      nrows = 1
    )
    
    expect_data_frame(
      exportFieldNames(rcon, 
                       fields = c("prereq_checkbox")), 
      ncols = 3, 
      nrows = 4
    )
  }
)

#####################################################################
# Argument Validation


test_that(
  "Return an error when rcon is not a redcapConnection object", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFieldNames("not an rcon"), 
                 "no applicable method for 'exportFieldNames'")
  }
)


test_that(
  "Return an error when fields is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(
      exportFieldNames(rcon, 
                       fields = 1:2), 
      "'fields'[:] Must be of type 'character'"
    )
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFieldNames(rcon, 
                                error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportFieldNames(rcon, 
                                config = list(1)), 
                 "'config': Must have names")
    expect_error(exportFieldNames(rcon, 
                                config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportFieldNames(rcon, 
                                api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportFieldNames(rcon, 
                                api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

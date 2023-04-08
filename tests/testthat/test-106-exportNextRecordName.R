context("exportNextRecordName.R")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Return the next record name", 
  {
    Records <- exportRecords(rcon, 
                             fields = "record_id")
    next_id <- max(as.numeric(Records$record_id)) + 1
    # Since the test data base can be expected to 
    expect_equal(exportNextRecordName(rcon), 
                 next_id)
  }
)


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
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportNextRecordName(rcon, 
                                      error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
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
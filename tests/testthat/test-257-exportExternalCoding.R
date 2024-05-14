#####################################################################
# Argument Validation                                            ####

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    expect_error(exportExternalCoding("not an rcon"), 
                 "no applicable method for 'exportExternalCoding'")
  }
)

test_that(
  "Return an error if fields is not character", 
  {
    expect_error(exportExternalCoding(rcon, 
                                      fields = 123), 
                 "Variable 'fields': Must be of type 'character'")
    
    expect_error(exportExternalCoding(rcon, 
                                      fields = "not a field"), 
                 "'fields': Must be a subset of")
  }
)

test_that(
  "Return an error if batch_size is not integerish(1)",
  {
    expect_error(exportExternalCoding(rcon, 
                                      batch_size = "1000"), 
                 "Variable 'batch_size': Must be of type 'integerish'")
    
    expect_error(exportExternalCoding(rcon, 
                                      batch_size = 1:2), 
                 "Variable 'batch_size': Must have length 1")
    
    expect_error(exportExternalCoding(rcon, 
                                      batch_size = -3), 
                 "Variable 'batch_size': Element 1 is not [>][=] 1")
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)

    expect_error(exportExternalCoding(rcon, 
                                      config = list(1)), 
                 "'config': Must have names")
    expect_error(exportExternalCoding(rcon, 
                                      config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportExternalCoding(rcon, 
                                      api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportExternalCoding(rcon, 
                                      api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# Functionality                                                  ####

# We have had difficulty getting coded and labeled values to work as expected
# when importing data as opposed to entering it through the UI. Manual tests 
# have been performed to confirm functionality, but we do not have 
# automated tests at this time.
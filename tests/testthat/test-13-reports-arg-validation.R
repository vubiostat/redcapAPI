context("Export Reports Argument Validation")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportReports("not an rcon"), 
                 "no applicable method for 'exportReports'")
  }
)

test_that(
  "Return an error if report_id is not integerish(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportReports(rcon, 
                               report_id = c(123, 456)), 
                 "'report_id': Must have length 1")
  }
)

test_that(
  "Return an error if factors is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportReports(rcon, 
                               report_id = 100, 
                               factors = c(TRUE, FALSE)), 
                 "'factors'[:] Must have length 1")
    expect_error(exportReports(rcon, 
                               report_id = 100, 
                               factors = "TRUE"), 
                 "Variable 'factors'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if labels is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportReports(rcon,
                               report_id = 100,  
                               labels = c(TRUE, FALSE)), 
                 "'labels'[:] Must have length 1")
    expect_error(exportReports(rcon, 
                               report_id = 100, 
                               labels = "TRUE"), 
                 "'labels'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if dates is not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportReports(rcon, 
                               report_id = 100, 
                               dates = c(TRUE, FALSE)), 
                 "'dates'[:] Must have length 1")
    expect_error(exportReports(rcon, 
                               report_id = 100, 
                               dates = "TRUE"), 
                 "'dates'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if drop is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportReports(rcon, 
                               report_id = 100,
                               drop = 1:3), 
                 "'drop': Must be of type 'character'")
  }
)

test_that(
  "Return an error if checkboxLabels is not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportReports(rcon, 
                               report_id = 100, 
                               checkboxLabels = c(TRUE, FALSE)), 
                 "'checkboxLabels'[:] Must have length 1")
    expect_error(exportReports(rcon, 
                               report_id = 100, 
                               checkboxLabels = "TRUE"), 
                 "'checkboxLabels'[:] Must be of type 'logical'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportReports(rcon, 
                               report_id = 100, 
                               error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportReports(rcon, 
                               report_id = 100, 
                               config = list(1)), 
                 "'config': Must have names")
    expect_error(exportReports(rcon, 
                               report_id = 100, 
                               config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportReports(rcon, 
                               report_id = 100, 
                               api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportReports(rcon, 
                               report_id = 100, 
                               api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

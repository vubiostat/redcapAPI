context("Export Reports Argument Validation")

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportReports("not an rcon", 
                               report_id = EXPORT_REPORTS_ID), 
                 "no applicable method for 'exportReports'")
  }
)

test_that(
  "Return an error if report_id is not integerish(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportReports(rcon, 
                               report_id = c(1, 2)), 
                 "Variable 'report_id': Must have length 1")
    expect_error(exportReports(rcon, 
                               report_id = TRUE), 
                 "'report_id': Must be of type 'integerish'")
  }
)

test_that(
  "Return an error if raw_or_label is not one of 'raw' or 'label'", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportReports(rcon, 
                               report_id = EXPORT_REPORTS_ID, 
                               raw_or_label = "coded"), 
                 "'raw_or_label': Must be element of set")
  }
)

test_that(
  "Return an error if raw_or_label_headers is not one of 'raw' or 'label'", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportReports(rcon, 
                               report_id = EXPORT_REPORTS_ID, 
                               raw_or_label_headers = "coded"), 
                 "'raw_or_label_headers': Must be element of set")
  }
)

test_that(
  "Return an error if export_checkbox_label is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportReports(rcon, 
                               report_id = EXPORT_REPORTS_ID, 
                               export_checkbox_label = "TRUE"), 
                 "'export_checkbox_label': Must be of type 'logical'")
    
    expect_error(exportReports(rcon, 
                               report_id = EXPORT_REPORTS_ID, 
                               export_checkbox_label = c(TRUE, FALSE)), 
                 "'export_checkbox_label': Must have length 1")
  }
)

test_that(
  "Return an error when csv_delimiter is not an acceptable value", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportReports(rcon, 
                               report_id = EXPORT_REPORTS_ID, 
                               csv_delimiter = "k"), 
                 "'csv_delimiter': Must be element of set")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportReports(rcon, 
                               report_id = EXPORT_REPORTS_ID, 
                               error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportReports(rcon,
                               report_id = EXPORT_REPORTS_ID,  
                               config = list(1)), 
                 "'config': Must have names")
    expect_error(exportReports(rcon, 
                               report_id = EXPORT_REPORTS_ID, 
                               config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportReports(rcon,
                               report_id = EXPORT_REPORTS_ID,  
                               api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportReports(rcon, 
                               report_id = EXPORT_REPORTS_ID, 
                               api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

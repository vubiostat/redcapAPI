context("Export Records Argument Validation")

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords("not an rcon"), 
                 "no applicable method for 'exportRecords'")
  }
)

test_that(
  "Return an error if records is not a character vector", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               records = c(TRUE, FALSE)), 
                 "'records': Must be of type 'character'")
  }
)

test_that(
  "Return an error if fields is not a character vector", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               fields = 1:3), 
                 "'fields': Must be of type 'character'")
  }
)

test_that(
  "Return an error if forms is not a character vector", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               forms = 1:3), 
                 "'forms': Must be of type 'character'")
  }
)

test_that(
  "Return an error if events is not a character vector", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               events = 1:3), 
                 "'events': Must be of type 'character'")
  }
)

test_that(
  "Return an error if raw_or_label is not one of 'raw' or 'label'", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               raw_or_label = "coded"), 
                 "'raw_or_label': Must be element of set")
  }
)

test_that(
  "Return an error if raw_or_label_headers is not one of 'raw' or 'label'", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               raw_or_label_headers = "coded"), 
                 "'raw_or_label_headers': Must be element of set")
  }
)

test_that(
  "Return an error if export_checkbox_label is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               export_checkbox_label = "TRUE"), 
                 "'export_checkbox_label': Must be of type 'logical'")
    
    expect_error(exportRecords(rcon, 
                               export_checkbox_label = c(TRUE, FALSE)), 
                 "'export_checkbox_label': Must have length 1")
  }
)

test_that(
  "Return an error if export_survey_fields is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               export_survey_fields = "TRUE"), 
                 "'export_survey_fields': Must be of type 'logical'")
    
    expect_error(exportRecords(rcon, 
                               export_survey_fields = c(TRUE, FALSE)), 
                 "'export_survey_fields': Must have length 1")
  }
)

test_that(
  "Return an error if export_dags is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               export_dags = "TRUE"), 
                 "'export_dags': Must be of type 'logical'")
    
    expect_error(exportRecords(rcon, 
                               export_dags = c(TRUE, FALSE)), 
                 "'export_dags': Must have length 1")
  }
)

test_that(
  "Return an error when csv_delimiter is not an acceptable value", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               csv_delimiter = "k"), 
                 "'csv_delimiter': Must be element of set")
  }
)

test_that(
  "Return an error if batch_size is not integerish(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               batch_size = pi), 
                 "'batch_size': Must be of type 'integerish'")
    
    expect_error(exportRecords(rcon, 
                               batch_size = 1:2), 
                 "'batch_size': Must have length 1")
    
    expect_error(exportRecords(rcon, 
                               batch_size = -1), 
                 "'batch_size': Element 1 is not [>][=] 1")
  }
)

# test_that(
#   "Validate error_handling, config, api_param", 
#   {
#     local_reproducible_output(width = 200)
#     expect_error(exportRecords(rcon, 
#                                error_handling = "not an option"), 
#                  "'error[_]handling': Must be element of set [{]'null','error'[}]")
#     
#     expect_error(exportRecords(rcon, 
#                                config = list(1)), 
#                  "'config': Must have names")
#     expect_error(exportRecords(rcon, 
#                                config = "not a list"), 
#                  "'config': Must be of type 'list'")
#     
#     expect_error(exportRecords(rcon, 
#                                api_param = list(1)), 
#                  "'api_param': Must have names")
#     expect_error(exportRecords(rcon, 
#                                api_param = "not a list"), 
#                  "'api_param': Must be of type 'list'")
#   }
# )

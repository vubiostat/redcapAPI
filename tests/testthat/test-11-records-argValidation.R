context("Export/Import/Delete Records Argument Validation")

#####################################################################
# exportRecords

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords("not an rcon"), 
                 "no applicable method for 'exportRecords")
  }
)

test_that(
  "Return an error if records is not character or numeric", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               records = c(TRUE, FALSE)), 
                 "'records': Must be of type 'character'")
  }
)

test_that(
  "Return an error if fields is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               fields = 1:4), 
                 "'fields': Must be of type 'character'")
  }
)

test_that(
  "Return an error if forms is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               forms = 1:4), 
                 "'forms': Must be of type 'character'")
  }
)

test_that(
  "Return an error if events is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               events = 1:4), 
                 "'events': Must be of type 'character'")
  }
)

test_that(
  "Return an error if raw_or_label arguments are not an acceptable value",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               raw_or_label = "something"), 
                 "'raw_or_label': Must be element of set")
    
    expect_error(exportRecords(rcon, 
                               raw_or_label_headers = "something"), 
                 "'raw_or_label_headers': Must be element of set")
  }
)

test_that(
  "Return an error if logical arguments are not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               export_checkbox_label = "TRUE"), 
                 "'export_checkbox_label': Must be of type 'logical'")
    expect_error(exportRecords(rcon, 
                               export_checkbox_label = c(TRUE, FALSE)), 
                 "'export_checkbox_label': Must have length 1")
    
    expect_error(exportRecords(rcon, 
                               export_survey_fields = "TRUE"), 
                 "'export_survey_fields': Must be of type 'logical'")
    expect_error(exportRecords(rcon, 
                               export_survey_fields = c(TRUE, FALSE)), 
                 "'export_survey_fields': Must have length 1")
    
    expect_error(exportRecords(rcon, 
                               export_dags = "TRUE"), 
                 "'export_dags': Must be of type 'logical'")
    expect_error(exportRecords(rcon, 
                               export_dags = c(TRUE, FALSE)), 
                 "'export_dags': Must have length 1")
  }
)

test_that(
  "Return an error if batch_size is not integerish(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               batch_size = -2), 
                 "Variable 'batch_size': Element 1 is not [>][=] 1")
    expect_error(exportRecords(rcon, 
                               batch_size = "1"), 
                 "'batch_size': Must be of type 'integerish'")
    expect_error(exportRecords(rcon, 
                               batch_size = c(10, 100)), 
                 "'batch_size': Must have length 1")
  }
)

test_that(
  "Return an error if csv_delimiter is not an acceptable value", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               csv_delimiter = "yup"), 
                 "'csv_delimiter': Must be element of set")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportRecords(rcon, 
                               config = list(1)), 
                 "'config': Must have names")
    expect_error(exportRecords(rcon, 
                               config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportRecords(rcon, 
                               api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportRecords(rcon, 
                               api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# importRecords

NewRecord <- data.frame(record_id = 1000:1003, 
                        no_prereq_checkbox___1 = rep(1, 4))

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords("not an rcon", 
                               data = NewRecord), 
                 "no applicable method for 'importRecords'")
  }
)

test_that(
  "Return an error if data is not a data.frame", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               data = "not a data frame"), 
                 "'data': Must be of type 'data.frame'")
  }
)

test_that(
  "Return an error if overwriteBehavior is not one of 'normal', 'overwrite'", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               overwriteBehavior = "not an option"), 
                 "'overwriteBehavior': Must be element of set [{]'normal','overwrite'[}]")
  }
)

test_that(
  "Return an error if returnContent is not one of 'count', 'ids', 'nothing', 'auto_ids'", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               returnContent = "not an option"), 
                 "'returnContent': Must be element of set [{]'count','ids','nothing','auto_ids'[}]")
  }
)

test_that(
  "Return an error if returnData is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               returnData = c(TRUE, FALSE)), 
                 "'returnData': Must have length 1")
    
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               returnData = "TRUE"), 
                 "'returnData': Must be of type 'logical'")
  }
)

test_that(
  "Return an error if logfile is not character(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               logfile = c("file1", "file2")), 
                 "'logfile': Must have length 1")
    
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               logfile = TRUE), 
                 "'logfile': Must be of type 'character'")
  }
)

test_that(
  "Return an error if force_auto_number is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               force_auto_number = c(TRUE, FALSE)), 
                 "'force_auto_number': Must have length 1")
    
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               force_auto_number = "TRUE"), 
                 "'force_auto_number': Must be of type 'logical'")
  }
)

test_that(
  "Return an error when batch.size is not integerish(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               batch.size = c(100, 200)), 
                 "'batch.size': Must have length 1")
    
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               batch.size = "-1"), 
                 "'batch.size': Must be of type 'integerish'")
    
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               config = list(1)), 
                 "'config': Must have names")
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# deleteRecords

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteRecords("not an rcon", 
                               records = 1000), 
                 "no applicable method for 'deleteRecords'")
  }
)

test_that(
  "Return an error if records is not character (or integerish)", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteRecords(rcon, 
                               c(TRUE, FALSE)), 
                 "Variable 'records'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if arms is not integerish(1) or character(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteRecords(rcon, 
                               records = 1000, 
                               arm = 1:2), 
                 "'arm': Must have length 1")
    
    expect_error(deleteRecords(rcon, 
                               records = 1000, 
                               arm = c(TRUE, FALSE)), 
                 "'arm': Must be of type 'integerish'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteRecords(rcon, 
                               records = 1000,
                               error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(deleteRecords(rcon, 
                               records = 1000,
                               config = list(1)), 
                 "'config': Must have names")
    expect_error(deleteRecords(rcon, 
                               records = 1000,
                               config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(deleteRecords(rcon, 
                               records = 1000,
                               api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(deleteRecords(rcon, 
                               records = 1000,
                               api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

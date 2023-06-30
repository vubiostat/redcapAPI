context("Export/Import/Delete Records Argument Validation")

#####################################################################
# exportRecords


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

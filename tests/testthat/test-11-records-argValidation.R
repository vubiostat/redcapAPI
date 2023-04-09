context("Export/Import/Delete Records Argument Validation")

rcon <- redcapConnection(url = url, token = API_KEY)

#####################################################################
# exportRecords

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords("not an rcon"), 
                 "no applicable method for 'exportRecords'")
  }
)

test_that(
  "Return an error if factors is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               factors = c(TRUE, FALSE)), 
                 "'factors'[:] Must have length 1")
    expect_error(exportRecords(rcon, factors = "TRUE"), 
                 "Variable 'factors'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if fields is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               fields = 1:2), 
                 "'fields'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if forms is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               fields = 1:2), 
                 "'fields'[:] Must be of type 'character'")
  }
)


test_that(
  "Return an error if records is not numeric or character", 
  {
    local_reproducible_output(width = 200)
    WithCharacter <- exportRecords(rcon, records = c("1", "2"))
    WithNumeric <- exportRecords(rcon, records = c(1, 2))
    
    expect_identical(WithCharacter, 
                     WithNumeric)
    expect_error(exportRecords(rcon, records = TRUE), 
                 "'records'[:] Must be of type 'character'")
  } 
)

test_that(
  "Return an error if events is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, events = 1), 
                 "'events'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if labels is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               labels = c(TRUE, FALSE)), 
                 "'labels'[:] Must have length 1")
    expect_error(exportRecords(rcon, 
                               labels = "TRUE"), 
                 "'labels'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if dates is not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               dates = c(TRUE, FALSE)), 
                 "'dates'[:] Must have length 1")
    expect_error(exportRecords(rcon, 
                               dates = "TRUE"), 
                 "'dates'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if drop is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               drop = 1:3), 
                 "'drop': Must be of type 'character'")
  }
)

test_that(
  "Return an error if survey is not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               survey = c(TRUE, FALSE)), 
                 "'survey'[:] Must have length 1")
    expect_error(exportRecords(rcon, 
                               survey = "TRUE"), 
                 "'survey'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if dag is not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               dag = c(TRUE, FALSE)), 
                 "'dag'[:] Must have length 1")
    expect_error(exportRecords(rcon, 
                               dag = "TRUE"), 
                 "'dag'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if checkboxLabels is not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               checkboxLabels = c(TRUE, FALSE)), 
                 "'checkboxLabels'[:] Must have length 1")
    expect_error(exportRecords(rcon, 
                               checkboxLabels = "TRUE"), 
                 "'checkboxLabels'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if colClasses is not a named vector", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(exportRecords(rcon, 
                               colClasses = list("character", "numeric", "character")))
    
    expect_error(exportRecords(rcon, 
                               colClasses = c("character", "numeric", "numeric")))
  }
)

test_that(
  "Return an error if batch.size is not integerish(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               batch.size = c(-1, 100)), 
                 "'batch.size'[:] Must have length 1")
    expect_error(exportRecords(rcon, 
                               batch.size = "-1"), 
                 "'batch.size'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error if form_complete_auto is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecords(rcon, 
                               form_complete_auto = c(TRUE, FALSE)), 
                 "'form_complete_auto'[:] Must have length 1")
    expect_error(exportRecords(rcon, 
                               form_complete_auto = "TRUE"), 
                 "'form_complete_auto'[:] Must be of type 'logical'")
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
  "Return an error if returnContent is not one of 'count', 'ids', 'nothing'", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               data = NewRecord, 
                               returnContent = "not an option"), 
                 "'returnContent': Must be element of set [{]'count','ids','nothing'[}]")
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

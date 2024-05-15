context("Import / Delete Records Methods Argument Validation")

load(file.path(test_path("testdata"),
               "test_redcapAPI_MetaData.Rdata"))
load(file.path(test_path("testdata"), 
               "test_redcapAPI_Data.Rdata"))

purgeProject(rcon, 
             purge_all = TRUE)

fields <- c("record_id", "letters_only_test", "number_test", "date_dmy_test", 
            "left_operand", "calc_squared")
MetaData <- test_redcapAPI_MetaData[test_redcapAPI_MetaData$field_name %in% fields, ]

ImportData <- test_redcapAPI_Data
ImportData <- ImportData[1, names(ImportData) %in% fields]

importMetaData(rcon, 
               MetaData)

importArms(rcon, 
           data = data.frame(arm_num = 1:2, 
                                  name = c("Arm 1", "Arm2")))
importEvents(rcon, 
             data = data.frame(event_name = c("Event 1", "Event 2"),
                                     arm_num = 1:2, 
                                     unique_event_name = c("event_1_arm_1", 
                                                           "event_1_arm_2")))

importProjectInformation(rcon, 
                         data.frame(is_longitudinal = 1))

importMappings(rcon, 
               data = data.frame(arm_num = rep(1, 5), 
                                 unique_event_name = rep("event_1_arm_1", 5), 
                                 form = rcon$instruments()$instrument_name))


#####################################################################
# Import Records Argument Validation                             ####

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords("not an rcon",
                               data = ImportData, 
                               skip_import = TRUE), 
                 "no applicable method for 'importRecords'")
  }
)


test_that(
  "Return an error if data is not a data frame", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               data = "not data", 
                               skip_import = TRUE), 
                 "'data': Must be of type 'data.frame'")
  }
)

test_that(
  "Error and stop if a field in data is not in meta data", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                                 data = data.frame(record_id = 1, 
                                                   not_a_field = "xyz"), 
                                 skip_import = TRUE), 
                   "variable[(]s[)] not_a_field are not found in the project")
  }
)

test_that(
  "Return an error if ID field can't be matched in the project", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               data = ImportData[-1], 
                               skip_import = TRUE), 
                 "The variable 'record_id' cannot be found in 'data'")
  }
)

test_that(
  "Print a message if the ID field is not the first field", 
  {
    local_reproducible_output(width = 200)
    expect_message(importRecords(rcon, 
                                 data = ImportData[c(2, 1, 3)], 
                                 skip_import = TRUE),
                   "The variable'record_id' was not in the first column.")
  }
)

test_that(
  "Print a message if calculated fields are included", 
  {
    local_reproducible_output(width = 200)
    expect_message(importRecords(rcon, 
                                 data = ImportData, 
                                 skip_import = TRUE), 
                   "calculated fields and cannot be imported")
  }
)

test_that(
  "Return an error if overwriteBehavior is not one of 'normal' or 'overwrite'", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               ImportData, 
                               overwrite_behavior = "something different", 
                               skip_import = TRUE), 
                 "'overwrite_behavior': Must be element of set [{]'normal','overwrite'[}]")
    
    expect_error(importRecords(rcon, 
                               ImportData, 
                               overwriteBehavior = "something different", 
                               skip_import = TRUE), 
                 "'overwrite_behavior': Must be element of set [{]'normal','overwrite'[}]")
  }
)

test_that(
  "Return an error if returnContent is not an accepted value", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               ImportData, 
                               return_content = "something different", 
                               skip_import = TRUE), 
                 "'return_content': Must be element of set")
    
    expect_error(importRecords(rcon, 
                               ImportData, 
                               returnContent = "something different", 
                               skip_import = TRUE), 
                 "'return_content': Must be element of set")
  }
)

test_that(
  "Return an error if skip_import is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               ImportData, 
                               skip_import = "TRUE"), 
                 " Variable 'skip_import': Must be of type 'logical'")
    
    expect_error(importRecords(rcon, 
                               ImportData, 
                               skip_import = c(TRUE, FALSE)), 
                 "Variable 'skip_import': Must have length 1")
  }
)

test_that(
  "Return an error if force_auto_number is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               ImportData, 
                               force_auto_number = "TRUE", 
                               skip_import = TRUE), 
                 " Variable 'force_auto_number': Must be of type 'logical'")
    
    expect_error(importRecords(rcon, 
                               ImportData, 
                               force_auto_number = c(TRUE, FALSE), 
                               skip_import = TRUE), 
                 "Variable 'force_auto_number': Must have length 1")
  }
)

test_that(
  "Return an error if batch_size is not integerish(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               ImportData, 
                               batch_size = "1"), 
                 "'batch_size': Must be of type 'integerish'")
    
    expect_error(importRecords(rcon, 
                               ImportData, 
                               batch_size = 1:2), 
                 "'batch_size': Must have length 1")
    
    expect_error(importRecords(rcon, 
                               ImportData, 
                               batch_size = pi), 
                 "'batch_size': Must be of type 'integerish'")
    
    
    
    
    expect_error(importRecords(rcon, 
                               ImportData, 
                               batch.size = "1"), 
                 "'batch_size': Must be of type 'integerish'")
    
    expect_error(importRecords(rcon, 
                               ImportData, 
                               batch.size = 1:2), 
                 "'batch_size': Must have length 1")
    
    expect_error(importRecords(rcon, 
                               ImportData, 
                               batch.size = pi), 
                 "'batch_size': Must be of type 'integerish'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(importRecords(rcon, 
                               data = ImportData,
                               error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(importRecords(rcon, 
                               data = ImportData,
                               config = list(1)), 
                 "'config': Must have names")
    expect_error(importRecords(rcon, 
                               data = ImportData,
                               config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(importRecords(rcon, 
                               data = ImportData,
                               api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(importRecords(rcon, 
                               data = ImportData,
                               api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# Delete Records Argument Validation                             ####
context("deleteRecords Argument Validation")

importRecords(rcon, ImportData)

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteRecords("not rcon", 
                               records = 1),
                 "no applicable method for 'deleteRecords'")
  }
)

test_that(
  "Return an error if records is neither character nor numeric", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteRecords(rcon, 
                               records = mtcars), 
                 "'records': Must be of type 'character'")
  }
)

test_that(
  "Return an error if instrument is not a character(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteRecords(rcon, 
                               records = 1,
                               instrument = TRUE), 
                 "'instrument': Must be of type 'character'")
    expect_error(deleteRecords(rcon, 
                           records = 1,
                           instrument = c("a", "b")), 
             "'instrument': Must have length 1")
  }
)

test_that(
  "Return an error if event is not a character(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteRecords(rcon, 
                               records = 1,
                               event = TRUE), 
                 "'event': Must be of type 'character'")
    expect_error(deleteRecords(rcon, 
                           records = 1,
                           event = c("a", "b")), 
             "'event': Must have length 1")
  }
)

test_that(
  "Return an error if repeat_instance is not a numeric(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteRecords(rcon, 
                               records = 1,
                               repeat_instance = TRUE), 
                 "'repeat_instance': Must be of type 'integerish'")
    expect_error(deleteRecords(rcon, 
                           records = 1,
                           repeat_instance = c(1,2)), 
             "'repeat_instance': Must have length 1")
  }
)

test_that(
  "Return an error if delete_logging is not a logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteRecords(rcon, 
                               records = 1,
                               delete_logging = 1), 
                 "'delete_logging': Must be of type 'logical'")
    expect_error(deleteRecords(rcon, 
                           records = 1,
                           delete_logging = c(TRUE, FALSE)), 
             "'delete_logging': Must have length 1")
  }
)

test_that(
  "Return an error if arm is not an arm in the project", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteRecords(rcon, 
                               records = 1, 
                               arm = 3), 
                 "Variable 'arm': Must be a subset of")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteRecords(rcon, 
                               records = 1,
                               error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(deleteRecords(rcon, 
                               records = 1,
                               config = list(1)), 
                 "'config': Must have names")
    expect_error(deleteRecords(rcon, 
                               records = 1,
                               config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(deleteRecords(rcon, 
                               records = 1,
                               api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(deleteRecords(rcon, 
                               records = 1,
                               api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)


purgeProject(rcon, 
             purge_all = TRUE)

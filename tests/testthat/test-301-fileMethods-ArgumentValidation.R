context("export/import/delete Files Argument Validation")

#####################################################################
# exportFiles

test_that(
  "exportFiles Argument Checking", 
  {
    local_reproducible_output(width = 200)
    # rcon is redcapApiConnection
    expect_error(exportFiles(rcon = mtcars), 
                 "no applicable method for 'exportFiles'")
    
    # record is not character (may also be numeric)
    expect_error(exportFiles(rcon,
                             record = TRUE, 
                             field = "field_name", 
                             dir = "dir"), 
                 "'record': Must be of type 'character'")
    
    # record is a character(1)
    expect_error(exportFiles(rcon, 
                             record = c("30", "31"), 
                             field = "field_name", 
                             dir = "dir"), 
                 "'record': Must have length 1")
    
    # field is character(1)
    expect_error(exportFiles(rcon, 
                             record = 1,
                             field = 1, 
                             dir = "dir"), 
                 "'field': Must be of type 'character'")
    
    expect_error(exportFiles(rcon,
                             record = 1, 
                             field = c("field1", "field2"), 
                             dir = "dir"), 
                 "'field': Must have length 1")    
    
    # event is character(1)
    expect_error(exportFiles(rcon, 
                             record = 1, 
                             field = "field", 
                             dir = "dir",
                             event = 1), 
                 "'event': Must be of type 'character'")
    
    expect_error(exportFiles(rcon, 
                             record = 1, 
                             field = "field", 
                             dir = "dir",
                             event = c("event1", "event2")), 
                 "'event': Must have length 1") 
    
    # dir is character(1)
    expect_error(exportFiles(rcon, 
                             record = 1, 
                             field = "field", 
                             dir = 123,
                             event = NULL), 
                 "'dir': Must be of type 'character'")
    
    expect_error(exportFiles(rcon, 
                             record = 1, 
                             field = "field", 
                             dir = c("dir1", "dir2"),
                             event = NULL), 
                 "'dir': Must have length 1") 
    
    # file_prefix is logical(1)
    expect_error(exportFiles(rcon, 
                             record = 1, 
                             field = "field", 
                             dir = "dir", 
                             event = NULL, 
                             file_prefix = c(TRUE, FALSE)), 
                 "'file_prefix': Must have length 1")
    
    expect_error(exportFiles(rcon, 
                             record = 1, 
                             field = "field", 
                             dir = "dir", 
                             event = NULL, 
                             file_prefix = "TRUE"), 
                 "'file_prefix': Must be of type 'logical'")
    
    # repeat_instance is integerish(1)
    expect_error(exportFiles(rcon, 
                             record = 1, 
                             field = "file_import_field", 
                             dir = "dir", 
                             event = NULL,
                             repeat_instance = pi), 
                 "'repeat_instance': Must be of type 'integerish'")
    
    expect_error(exportFiles(rcon, 
                             record = 1, 
                             field = "file_import_field", 
                             dir = "dir", 
                             event = NULL,
                             repeat_instance = c(1, 2)), 
                 "'repeat_instance': Must have length 1") 
  }
)

#####################################################################
# importFiles

test_that(
  "importFiles Argument Checking", 
  {
    local_reproducible_output(width = 200)
    # rcon is redcapApiConnection
    expect_error(importFiles(rcon = mtcars), 
                 "no applicable method for 'importFiles'")
    
    # file is a character(1)
    expect_error(importFiles(rcon, 
                             file = 123, 
                             record = "30", 
                             field = "field_name"), 
                 "'file': Must be of type 'character'")
    
    expect_error(importFiles(rcon, 
                             file = c("file1", "file2"), 
                             record = "30", 
                             field = "field_name"), 
                 "'file': Must have length 1")
    
    # record is not character (may also be numeric)
    expect_error(importFiles(rcon,
                             record = TRUE,
                             field = "fieldname",
                             file = "filename"), 
                 "'record': Must be of type 'character'")
    
    # record is a character(1)
    expect_error(importFiles(rcon, 
                             record = c("30", "31"), 
                             field = "field_name", 
                             file = "filename"), 
                 "'record': Must have length 1")
    
    # field is character(1)
    expect_error(importFiles(rcon, 
                             record = 1,
                             field = 1, 
                             file = "filename"), 
                 "'field': Must be of type 'character'")
    
    expect_error(importFiles(rcon,
                             record = 1, 
                             field = c("field1", "field2"), 
                             file = "filename"), 
                 "'field': Must have length 1")    
    
    # event is character(1)
    expect_error(importFiles(rcon, 
                             record = 1, 
                             field = "field", 
                             file = "filename",
                             event = 1), 
                 "'event': Must be of type 'character'")
    
    expect_error(importFiles(rcon, 
                             record = 1, 
                             field = "field", 
                             file = "filename",
                             event = c("event1", "event2")), 
                 "'event': Must have length 1") 
    
    
    # overwrite is logical(1)
    expect_error(importFiles(rcon, 
                             record = 1, 
                             field = "field", 
                             event = NULL,
                             file = "filename",
                             overwrite = c(TRUE, FALSE)), 
                 "'overwrite': Must have length 1")
    
    expect_error(importFiles(rcon, 
                             record = 1, 
                             field = "field", 
                             dir = "dir", 
                             event = NULL,
                             file = "filename",
                             overwrite = "TRUE"), 
                 "'overwrite': Must be of type 'logical'")
    
    # repeat_instance is integerish(1)
    expect_error(importFiles(rcon, 
                             record = 1, 
                             field = "file_import_field", 
                             file = "filename",
                             event = NULL,
                             repeat_instance = pi), 
                 "'repeat_instance': Must be of type 'integerish'")
    
    expect_error(importFiles(rcon, 
                             record = 1, 
                             field = "file_import_field", 
                             file = "filename",
                             event = NULL,
                             repeat_instance = c(1, 2)), 
                 "'repeat_instance': Must have length 1") 
  }
)


#####################################################################
# deleteFiles

test_that(
  "deleteFiles Argument Checking", 
  {
    local_reproducible_output(width = 200)
    # rcon is redcapApiConnection
    expect_error(deleteFiles(rcon = mtcars), 
                 "no applicable method for 'deleteFiles'")
    
    # record is not character (may also be numeric)
    expect_error(deleteFiles(rcon, 
                             record = TRUE), 
                 "'record': Must be of type 'character'")
    
    # record is a character(1)
    expect_error(deleteFiles(rcon, 
                             record = c("30", "31")), 
                 "'record': Must have length 1")
    
    # field is character(1)
    expect_error(deleteFiles(rcon, 
                             field = 1), 
                 "'field': Must be of type 'character'")
    
    expect_error(deleteFiles(rcon, 
                             field = c("field1", "field2")), 
                 "'field': Must have length 1")    
    
    # event is character(1)
    expect_error(deleteFiles(rcon, 
                             event = 1), 
                 "'event': Must be of type 'character'")
    
    expect_error(deleteFiles(rcon, 
                             event = c("event1", "event2")), 
                 "'event': Must have length 1") 
    
    # repeat_instance is integerish(1)
    expect_error(deleteFiles(rcon, 
                             repeat_instance = pi), 
                 "'repeat_instance': Must be of type 'integerish'")
    
    expect_error(deleteFiles(rcon, 
                             repeat_instance = c(1, 2)), 
                 "'repeat_instance': Must have length 1") 
  }
)

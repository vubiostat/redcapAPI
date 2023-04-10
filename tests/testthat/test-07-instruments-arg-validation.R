context("instruments, import/export mappings, export PDF argument validations")

rcon <- redcapConnection(url = url, token = API_KEY)

#####################################################################
# exportInstruments

test_that(
  "Return an error when rcon is not a redcapConnection object", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportInstruments("not an rcon"), 
                 "no applicable method for 'exportInstruments'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportInstruments(rcon, 
                                   error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportInstruments(rcon, 
                                   config = list(1)), 
                 "'config': Must have names")
    expect_error(exportInstruments(rcon, 
                                   config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportInstruments(rcon, 
                                   api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportInstruments(rcon, 
                                   api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# exportMappings

test_that(
  "Return an error when rcon is not a redcapConnection object", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportMappings("not an rcon"), 
                 "no applicable method for 'exportMappings'")
  }
)

test_that(
  "Return an error when arms is not character",
  {
    local_reproducible_output(width = 200)
    expect_error(exportMappings(rcon, 
                                arms = 1:2), 
                 "Variable 'arms'[:] Must be of type 'character'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportMappings(rcon, 
                                error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportMappings(rcon, 
                                config = list(1)), 
                 "'config': Must have names")
    expect_error(exportMappings(rcon, 
                                config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportMappings(rcon, 
                                api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportMappings(rcon, 
                                api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# importMappings

# FIXME: Add tests after writing file

#####################################################################
# exportPDF

test_that(
  "exportPDF Argument validation", 
  {
    local_reproducible_output(width = 200)
    # Validate redcapConnection
    expect_error(exportPdf("not an rcon", 
                           dir = tempdir()), 
                 "no applicable method for 'exportPdf'")
    
    # dir must be a character(1), and be a directory that exists
    expect_error(exportPdf(rcon, 
                           dir = 123), 
                 "Variable 'dir': Must be of type 'character'")
    expect_error(exportPdf(rcon, 
                           dir = c("dir1", "dir2")), 
                 "'dir': Must have length 1")
    expect_error(exportPdf(rcon, 
                           record = 10, 
                           events = "event",
                           instruments = "instrument",
                           dir = "folder no exist"), 
                 "'dir': Directory 'folder no exist'")
    
    # filename must be character(1)
    expect_error(exportPdf(rcon, 
                           dir = tempdir(), 
                           filename = 123), 
                 "Variable 'filename': Must be of type 'character'")
    expect_error(exportPdf(rcon, 
                           dir = tempdir(), 
                           filename = c("file1", "file2")), 
                 "'filename': Must have length 1")
    
    # record must be character(1)
    expect_error(exportPdf(rcon, 
                           dir = tempdir(), 
                           record = TRUE), 
                 "Variable 'record': Must be of type 'character'")
    expect_error(exportPdf(rcon, 
                           dir = tempdir(), 
                           record = c("1", "2")), 
                 "'record': Must have length 1")
    
    # events must be character(1)
    expect_error(exportPdf(rcon, 
                           dir = tempdir(), 
                           record = 10, 
                           events = 123), 
                 "Variable 'events': Must be of type 'character'")
    expect_error(exportPdf(rcon, 
                           dir = tempdir(), 
                           record = 10, 
                           events = c("event1", "event2")), 
                 "'events': Must have length 1")
    
    # instruments must be character(1)
    expect_error(exportPdf(rcon, 
                           dir = tempdir(), 
                           record = 10, 
                           events = "event_1_arm_1", 
                           instruments = 123), 
                 "Variable 'instruments': Must be of type 'character'")
    expect_error(exportPdf(rcon, 
                           dir = tempdir(), 
                           record = 10, 
                           events = "event_1_arm_1", 
                           instruments = c("instrument1", "instrument2")), 
                 "'instruments': Must have length 1")
    
    # allRecords must be logical(1)
    
    expect_error(exportPdf(rcon, 
                           dir = tempdir(), 
                           record = 10, 
                           events = "event_1_arm_1", 
                           instruments = "branching_logic", 
                           all_records = c(TRUE, FALSE)), 
                 "'all_records': Must have length 1")
    
    expect_error(exportPdf(rcon, 
                           dir = tempdir(), 
                           record = 10, 
                           events = "event_1_arm_1", 
                           instruments = "branching_logic", 
                           all_records = "TRUE"), 
                 "'all_records': Must be of type 'logical'")
    
    # error_handling, config, api_param
    
    expect_error(exportPdf(rcon, 
                           dir = tempdir(), 
                           record = 10, 
                           events = "event_1_arm_1", 
                           instruments = "branching_logic",
                           error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportPdf(rcon, 
                           dir = tempdir(), 
                           record = 10, 
                           events = "event_1_arm_1", 
                           instruments = "branching_logic",
                           config = list(1)), 
                 "'config': Must have names")
    expect_error(exportPdf(rcon, 
                           dir = tempdir(), 
                           record = 10, 
                           events = "event_1_arm_1", 
                           instruments = "branching_logic",
                           config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportPdf(rcon, 
                           dir = tempdir(), 
                           record = 10, 
                           events = "event_1_arm_1", 
                           instruments = "branching_logic",
                           api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportPdf(rcon, 
                           dir = tempdir(), 
                           record = 10, 
                           events = "event_1_arm_1", 
                           instruments = "branching_logic",
                           api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

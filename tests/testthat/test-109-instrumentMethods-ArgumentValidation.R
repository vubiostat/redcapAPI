context("Instruments, Mappings, Export PDF Argument Validations")


#####################################################################
# exportInstruments                                              ####

test_that(
  "Return an error when rcon is not a redcapConnection object", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportInstruments("not an rcon"), 
                 "no applicable method for 'exportInstruments'")
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)
   
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
# exportMappings                                                 ####

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

#####################################################################
# importMappings                                                 ####

# Set up arms and events for testing importMappings data
load(file.path(test_path("testdata", 
                         "test_redcapAPI_MetaData.Rdata")))

Arms <- data.frame(arm_num = 1:2, 
                   name = c("Arm 1", "Arm 2"), 
                   stringsAsFactors = FALSE)

Events <- data.frame(event_name = c("event_1", 
                                    "event_2", 
                                    "event_1"), 
                     arm_num = c(1, 1, 2), 
                     unique_event_name = c("event_1_arm_1", 
                                           "event_2_arm_2", 
                                           "event_1_arm_2"), 
                     stringsAsFactors = FALSE)


importMetaData(rcon, 
               test_redcapAPI_MetaData)

importArms(rcon, 
           data = Arms)

importEvents(rcon, 
             data = Events)

importProjectInformation(rcon, 
                         data = data.frame(is_longitudinal = 1))


test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(importMappings(rcon = "not a connection", 
                                data = rcon$mapping()), 
                 "no applicable method for 'importMappings'")
    expect_error(importMappings.redcapApiConnection(rcon = "not a connection", 
                                                    data = rcon$mapping()), 
                 "'rcon': Must inherit from class 'redcapApiConnection'")
  }
)

test_that(
  "Return an error if data is not a data frame", 
  {
    local_reproducible_output(width = 200)
    expect_error(importMappings(rcon = rcon, 
                                data = "not a data frame"), 
                 "'data': Must be of type 'data.frame'")
  }
)

test_that(
  "Validate the content of data", 
  {
    local_reproducible_output(width = 200)
    
    # must have the right names
    NewMap <- data.frame(arm_num = c(1, 1, 2), 
                         unique_event_name = c("event_1_arm_1", 
                                               "event_2_arm_1", 
                                               "event_1_arm_2"), 
                         form = c("record_id", 
                                  "text_fields", 
                                  "record_id"))
    names(NewMap)[1] <- "the_arm_number"
    
    expect_error(importMappings(rcon = rcon, 
                                data = NewMap), 
                 "'names[(]data[)]': Must be a subset of [{]'arm_num','unique_event_name','form'[}]")

    names(NewMap)[1] <- "arm_num"
    NewMap$arm_num <- NewMap$arm_num + 3
    expect_error(importMappings(rcon = rcon,
                                data = NewMap),
                 "'data[$]arm_num': Must be a subset of [{]'1','2'[}]")

    NewMap$arm_num <- NewMap$arm_num - 3
    NewMap$unique_event_name <- sub("event", "different event", NewMap$unique_event_name)
    expect_error(importMappings(rcon = rcon,
                                data = NewMap),
                 "'data[$]unique_event_name': Must be a subset of [{]'event_1_arm_1','event_2_arm_1','event_1_arm_2'[}]")
    
    NewMap$unique_event_name <- sub("different event", "event", NewMap$unique_event_name)
    NewMap$form[1] <- "not_a_real_form"
    expect_error(importMappings(rcon = rcon,
                                data = NewMap),
                 "Variable 'data[$]form': Must be a subset of")
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)
   
    expect_error(importMappings(rcon, 
                                data = rcon$mapping(),
                                config = list(1)), 
                 "'config': Must have names")
    expect_error(importMappings(rcon, 
                                data = rcon$mapping(),
                                config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(importMappings(rcon, 
                                data = rcon$mapping(),
                                api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(importMappings(rcon,
                                data = rcon$mapping(),
                                api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

# Remove Arms and Mappings (cleanup)

purgeProject(rcon, 
             purge_all = TRUE)

#####################################################################
# exportPDF                                                      ####

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

  }
)

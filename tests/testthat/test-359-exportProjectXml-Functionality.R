context("Export Project XML Functionality")

xml_file <- tempfile(fileext = ".xml")

test_that(
  "Export Project XML", 
  {
    expect_true(exportProjectXml(rcon, 
                                 file = xml_file, 
                                 return_metadata_only = TRUE))
    
    expect_true(exportProjectXml(rcon, 
                                 file = xml_file, 
                                 return_metadata_only = FALSE))
    
    expect_true(exportProjectXml(rcon, 
                                 file = xml_file, 
                                 records = c("1", "2", "3")))
    
    fields <- rcon$metadata()$field_name[1:4]
    
    expect_true(exportProjectXml(rcon, 
                                 file = xml_file, 
                                 fields = fields))
    
    events <- rcon$events()$unique_event_name[1]
    
    expect_true(exportProjectXml(rcon, 
                                 file = xml_file, 
                                 events = events))
    
    expect_true(exportProjectXml(rcon, 
                                 file = xml_file, 
                                 survey = TRUE))
    
    expect_true(exportProjectXml(rcon, 
                                 file = xml_file, 
                                 dag = TRUE))
    
    expect_true(exportProjectXml(rcon, 
                                 file = xml_file, 
                                 export_files = TRUE))
    
    expect_true(exportProjectXml(rcon, 
                                 file = xml_file, 
                                 return_metadata_only = FALSE, 
                                 api_param = list(filterLogic = "[number_test] > 20")))
    
  }
)

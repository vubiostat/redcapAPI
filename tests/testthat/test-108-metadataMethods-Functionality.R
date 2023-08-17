context("Meta Data Methods Functionality")



load(test_path("testdata", "test_redcapAPI_MetaData.Rdata"))

test_that(
  "Import and Export Meta Data", 
  {
 
    MetaData <- test_redcapAPI_MetaData
    
    orig_instrument <- unique(MetaData$form_name)
    
    expect_message(importMetaData(rcon = rcon, 
                                  data = MetaData), 
                   sprintf("Fields Imported: %s", 
                           nrow(MetaData)))
    
    
    expect_data_frame(rcon$metadata(), 
                      nrows = nrow(MetaData))
    
    expect_equal(rcon$instruments()$instrument_name, 
                 orig_instrument)
    
    
    NextMetaData <- MetaData[1:10, ]
    
    # Verify behaviors under refresh = FALSE
    expect_message(importMetaData(rcon, 
                                  NextMetaData, 
                                  refresh = FALSE), 
                   "Fields Imported: 10")
    
    expect_data_frame(rcon$metadata(), 
                      nrows = nrow(MetaData))
    
    expect_equal(rcon$instruments()$instrument_name, 
                 orig_instrument)
    
    rcon$refresh_metadata()
    rcon$refresh_instruments()
    
    expect_data_frame(rcon$metadata(), 
                      nrows = nrow(NextMetaData))
    
    expect_equal(rcon$instruments()$instrument_name, 
                 unique(NextMetaData$form_name))
    
    # Clean up 
    
    importMetaData(rcon, 
                   data = MetaData[1, ])
  }
)

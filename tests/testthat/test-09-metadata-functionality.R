context("Export / Import MetaData Functionality")

# FIXME: realign tests when importMetaData is written

#####################################################################
# Export Meta Data

test_that(
  "Return a data frame when called with defaults", 
  {
    expect_data_frame(exportMetaData(rcon), 
                      ncols = 18)
  }
)

test_that(
  "Return a data frame of only selected fields", 
  {
    expect_data_frame(
      exportMetaData(rcon, 
                     fields = c("record_id", "date_dmy", "prereq_radio")), 
      ncols = 18, 
      nrows = 3
    )
  }
)

test_that(
  "Return a data frame of only selected forms", 
  {
    expect_data_frame(
      exportMetaData(rcon, 
                     forms = c("fieldtovar_datetimes", "randomization")), 
      ncols = 18, 
      nrows = 14
    )
  }
)

#####################################################################
# Import Meta Data

test_that(
  "Import a MetaData Data Frame", 
  {
    OrigMetaData <- rcon$metadata()
    OrigInstrument <- rcon$instruments()
    
    ImportMeta <- OrigMetaData[1:4, ]
    
    expect_message(importMetaData(rcon, 
                                  ImportMeta), 
                   "Fields Imported: 4")
    
    expect_data_frame(rcon$metadata(), 
                      nrows = 4)
    
    expect_data_frame(rcon$instruments(), 
                      nrows = 1)
    
    expect_message(importMetaData(rcon, 
                                  OrigMetaData, 
                                  refresh = FALSE), 
                   sprintf("Fields Imported: %s", nrow(OrigMetaData)))
    
    expect_data_frame(rcon$metadata(), 
                      nrows = 4)
    
    expect_data_frame(rcon$instruments(), 
                      nrows = 1)
    
    rcon$refresh_metadata()
    rcon$refresh_instruments()
    
    expect_data_frame(rcon$metadata(), 
                      nrows = nrow(OrigMetaData))
    
    expect_data_frame(rcon$instruments(), 
                      nrows = nrow(OrigInstrument))
  }
)

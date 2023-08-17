context("Export Records Functionality with Repeating Instruments")

#####################################################################
# Prepare data with Repeating Instruments                        ####

load(file.path(test_path("testdata"),
               "test_redcapAPI_MetaData.Rdata"))
load(file.path(test_path("testdata"), 
               "test_redcapAPI_Data.Rdata"))

importMetaData(rcon, 
               test_redcapAPI_MetaData)

rcon$refresh_instruments()
rcon$refresh_fieldnames()

forms <- rcon$instruments()$instrument_name
Mappings <- data.frame(arm_num = rep(1, length(forms)), 
                       unique_event_name = rep("event_1_arm_1", length(forms)), 
                       form = forms)
importMappings(rcon, 
               data = Mappings)

RepeatInst <- data.frame(event_name = "event_1_arm_1", 
                         form_name = "repeating_instrument")

importRepeatingInstrumentsEvents(rcon, 
                                 data = RepeatInst)

ImportData <- castForImport(test_redcapAPI_Data, 
                            rcon)

importRecords(rcon, ImportData)

#######################################################################
# Export Records with Repeating Instruments                        ####

test_that(
  "Exported data has repeating instruments", 
  {
    Rec <- exportRecords(rcon)
    expect_true(any(!is.na(Rec$redcap_repeat_instrument)))
    expect_true(any(!is.na(Rec$redcap_repeat_instrument)))
  }
)

test_that(
  "Repeating Instrument name can be labelled or raw", 
  {
    Rec <- exportRecords(rcon)
    
    expect_true(all(Rec$redcap_repeat_instrument %in% 
                      c(NA, rcon$instruments()$instrument_name)))
    
    
    
    Rec <- exportRecordsTyped(rcon, 
                              raw_or_label = "label")
    
    expect_true(all(Rec$redcap_repeat_instrument %in% 
                      c(NA, rcon$instruments()$instrument_label)))
  }
)

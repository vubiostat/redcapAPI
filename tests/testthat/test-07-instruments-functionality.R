context("instruments, import/export mappings, export PDF argument functionality")

#####################################################################
# exportInstruments                                              ####

test_that(
  "Returns a data frame of instruments", 
  {
    expect_data_frame(exportInstruments(rcon), 
                      ncols = 2)
  }
)

#####################################################################
# exportMappings                                                 ####

test_that(
  "Return a data frame of mappings when called with defaults (longitudinal)", 
  {
    is_longitudinal <- 
      as.logical(exportProjectInformation(rcon)$is_longitudinal)
    
    skip_if(!is_longitudinal, 
            "Test project is not a longitudinal project. This test is skipped")
    
    expect_data_frame(exportMappings(rcon), 
                      ncols = 3)
  }
)

test_that(
  "Return NULL for a classial project", 
  {
    # We will alter the project information and push it into the 
    # cached value in order to mimic the state of a non-longitudinal project
    # This only works because the API isn't every called for a non-longitudinal project
    importProjectInformation(rcon, 
                             data.frame(is_longitudinal = 0))
    rcon$flush_projectInformation()
    # tmp_proj <- rcon$projectInformation()
    # tmp_proj$is_longitudinal <- 0
    
    expect_data_frame(exportMappings(rcon), 
                      ncols = 3, 
                      nrows = 0)
    importProjectInformation(rcon, 
                             data.frame(is_longitudinal = 1))
    rcon$flush_projectInformation()
  }
)

#####################################################################
# importMappings

test_that(
  "Import Instrument Mappings Successfully", 
  {
    OrigMapping <- rcon$mapping()
    
    ArmOneMapping <- OrigMapping[OrigMapping$arm_num == 1, ]
    
    # Import a subset of mappings
    expect_message(importMappings(rcon, 
                                  ArmOneMapping), 
                   sprintf("Mappings imported: %s", nrow(ArmOneMapping)))
    
    expect_equal(ArmOneMapping, 
                 rcon$mapping())
    
    expect_message(importMappings(rcon, 
                                  OrigMapping, 
                                  refresh = FALSE), 
                   sprintf("Mappings imported: %s", nrow(OrigMapping)))
    
    expect_data_frame(rcon$mapping(), 
                      nrows = nrow(ArmOneMapping))
    
    rcon$refresh_mapping()
    
    expect_equal(OrigMapping, 
                 rcon$mapping())
  }
)

#####################################################################
# exportPDF                                                      ####

test_that(
  "Blank data collection sheet", 
  {
    expect_message(exportPdf(rcon, 
                             dir = tempdir(), 
                             events = "event_1_arm_1", 
                             instruments = "branching_logic"), 
                   "The file was saved to.+blank.pdf")
  }
)

test_that(
  "Download all instrument forms (blank)", 
  {
    expect_message(exportPdf(rcon, 
                             dir = tempdir(), 
                             events = "event_1_arm_1"), 
                   "The file was saved to.+blank.pdf")
  }
)

test_that(
  "exportPdf export the instrument PDF for a record",
  {
    expect_message(exportPdf(rcon, 
                             dir = tempdir(), 
                             record = 10, 
                             events = "event_1_arm_1", 
                             instruments = "branching_logic"), 
                   "The file was saved to.+record[_]10.pdf")
  }
)

test_that(
  "exportPdf export all instruments for a record",
  {
    expect_message(exportPdf(rcon, 
                             dir = tempdir(), 
                             record = 10, 
                             events = "event_1_arm_1"), 
                   "The file was saved to.+record[_]10.pdf")
  }
)

test_that(
  "all instruments with data from all records",
  {
    expect_message(exportPdf(rcon, 
                             dir = tempdir(), 
                             all_records = TRUE), 
                   "The file was saved to.+all[_]records.pdf")
  }
)


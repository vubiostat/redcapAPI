context("Instruments, Mappings, Export PDF Argument Functionality")

# Set up arms and events for testing importMappings data
load(file.path(test_path("testdata",
                         "test_redcapAPI_MetaData.Rdata")))
load(file.path(test_path("testdata",
                         "test_redcapAPI_Data.Rdata")))

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
purgeProject(rcon, purge_all = TRUE)
importMetaData(rcon, test_redcapAPI_MetaData)
importArms(rcon, data = Arms)
importEvents(rcon, data = Events)
importProjectInformation(rcon, data = data.frame(is_longitudinal = 1))

Mapping <- data.frame(arm_num = c(1, 1, 1, 2, 2, 2),
                      unique_event_name = c("event_1_arm_1",
                                            "event_1_arm_1",
                                            "event_2_arm_1",
                                            "event_1_arm_2",
                                            "event_1_arm_2",
                                            "event_1_arm_2"),
                      form = c("record_id",
                               "text_fields",
                               "multiple_choice",
                               "record_id",
                               "numbers",
                               "branching_logic"),
                      stringsAsFactors = FALSE)

RecordToImport <- test_redcapAPI_Data[test_redcapAPI_Data$record_id %in% 1:3, ]
RecordToImport <- RecordToImport[is.na(RecordToImport$repeat_question_1), ]
RecordToImport <- RecordToImport[names(RecordToImport) %in% rcon$metadata()$field_name]
# castForImport only needed until 3.0.0
RecordToImport <- castForImport(RecordToImport, rcon,
                                validation = list(bioportal = valSkip),
                                cast = list(number_1dp = as.numeric,
                                            number_2dp = as.numeric,
                                            number_1dp_comma_decimal = as.numeric,
                                            number_2dp_comma_decimal = as.numeric,
                                            bioportal = as.character))
importRecords(
  rcon,
  RecordToImport[,!names(RecordToImport) %in% c("signature_test", "file_upload_test"),drop=FALSE])

#####################################################################
# exportInstruments                                              ####

test_that(
  "Returns a data frame of instruments",
  {
    expect_data_frame(exportInstruments(rcon),
                      ncols = 2)
  }
)

test_that(
  "Validate config, api_param",
  {
    local_reproducible_output(width = 200)

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
    # tmp_proj <- rcon$projectInformation()
    # tmp_proj$is_longitudinal <- 0

    expect_data_frame(exportMappings(rcon),
                      ncols = 3,
                      nrows = 0)
    importProjectInformation(rcon,
                             data.frame(is_longitudinal = 1))
  }
)

#####################################################################
# importMappings                                                 ####

test_that(
  "Import Instrument Mappings Successfully",
  {
    ArmOneMapping <- Mapping[Mapping$arm_num == 1, ]

    # Import a subset of mappings
    n_imported <- importMappings(rcon,
                                 ArmOneMapping)
    expect_equal(n_imported, as.character(nrow(ArmOneMapping)))
    expect_equal(ArmOneMapping, rcon$mapping())
  }
)

#####################################################################
# exportPDF                                                      ####

test_that(
  "Blank data collection sheet",
  {
    temp_dir <- tempdir()
    save_to <- exportPdf(rcon,
                         dir = temp_dir,
                         events = "event_1_arm_1",
                         instruments = "multiple_choice")
    expect_equal(save_to,
                 file.path(temp_dir,
                           "redcap_forms_download_blank.pdf"))

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

test_that(
  "Download all instrument forms (blank)",
  {
    temp_dir <- tempdir()
    save_to <- exportPdf(rcon,
                         dir = temp_dir,
                         events = "event_1_arm_1")
    expect_equal(save_to,
                 file.path(temp_dir,
                           "redcap_forms_download_blank.pdf"))
  }
)

test_that(
  "exportPdf export the instrument PDF for a record",
  {
    temp_dir <- tempdir()
    save_to <- exportPdf(rcon,
                         dir = temp_dir,
                         record = 1,
                         events = "event_1_arm_1",
                         instruments = "multiple_choice")
    expect_equal(save_to,
                 file.path(temp_dir,
                           "redcap_forms_download_record_1.pdf"))
  }
)

test_that(
  "exportPdf export all instruments for a record",
  {
    temp_dir <- tempdir()
    save_to <- exportPdf(rcon,
                         dir = temp_dir,
                         record = 1,
                         events = "event_1_arm_1")
    expect_equal(save_to,
                 file.path(temp_dir,
                           "redcap_forms_download_record_1.pdf"))
  }
)

test_that(
  "all instruments with data from all records",
  {
    temp_dir <- tempdir()
    save_to <- exportPdf(rcon,
                         dir = temp_dir,
                         all_records = TRUE)
    expect_equal(save_to,
                 file.path(temp_dir,
                           "redcap_forms_download_all_records.pdf"))
  }
)


#####################################################################
# Cleanup                                                        ####

purgeProject(rcon, purge_all = TRUE)

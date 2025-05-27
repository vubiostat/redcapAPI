context("Export Typed Records Offline Functionality")

url     <- rcon$url   # Should not be required but it is

load(file.path(test_path("testdata"), "RedcapProject_test_redcapAPI.Rdata"))

suppressWarnings({
  roff <- offlineConnection(meta_data = RedcapProject_test_redcapAPI$meta_data, 
                            arms = RedcapProject_test_redcapAPI$arms, 
                            events = RedcapProject_test_redcapAPI$events, 
                            mapping = RedcapProject_test_redcapAPI$mappings, 
                            project_info = RedcapProject_test_redcapAPI$project_information, 
                            repeat_instrument = RedcapProject_test_redcapAPI$repeating_instruments, 
                            records = RedcapProject_test_redcapAPI$records)
})

test_that(
  "Data are returned", 
  {
    expect_data_frame(exportRecordsTyped(roff))
  }
)

test_that(
  "Only requested fields are returned", 
  {
    these_fields <-  c("record_id", "redcap_event_name", "redcap_repeat_instrument", 
                       "redcap_repeat_instance", "redcap_data_access_group",
                       "dropdown_test", "integer_test")
    Rec <- exportRecordsTyped(roff, 
                              fields = these_fields)
    expect_true(all(c(these_fields) %in% names(Rec)))
  }
)

test_that(
  "Drop fields are removed", 
  {
    Rec <- exportRecordsTyped(roff, 
                              drop_fields = "number_test")
    expect_false("number_test" %in% names(Rec))
  }
)

test_that(
  "Only fields in requested forms are returned", 
  {
    Rec <- exportRecordsTyped(roff, 
                              forms = "multiple_choice")
    expect_equal(names(Rec), 
                 c("record_id", "redcap_event_name", "redcap_data_access_group", 
                   "redcap_repeat_instrument", "redcap_repeat_instance", "checkbox_test___x", 
                   "checkbox_test___y", "checkbox_test___z", "dropdown_test", "radio_test", 
                   "truefalse_test", "yesno_test", "multiple_choice_complete"))
  }
)

test_that(
  "Only fields in requested events are returned", 
  {
    Rec <- exportRecordsTyped(roff, 
                              events = "event_2_arm_2")
    expect_data_frame(Rec, 
                      nrows = 0)
  }
)


# Validation failures and their links


offline_files <- test_path("testdata/offlineConnectionFiles")
test_that(
  "Invalid data is reported, links not generated with no event IDs", 
  {
    rcon_off <- 
      expect_warning(
        offlineConnection(
          meta_data = file.path(offline_files, "TestRedcapAPI_DataDictionary.csv"), 
          records = file.path(offline_files, "TestRedcapAPI_Records.csv"), 
          events = file.path(offline_files, "TestRedcapAPI_Events.csv"),
          project_info = data.frame(project_id = rcon$projectInformation()$project_id, 
                                    is_longitudinal = 1), 
          url = url, 
          version = rcon$version()
        )
      )
    
    Records <- expect_warning(exportRecordsTyped(rcon_off, 
                                                 cast = list(system = as.character)), 
                              "Some records failed validation")
    
    Inv <- reviewInvalidRecords(Records)
    
    expect_equal(Inv$link_to_form, 
                 c(NA_character_, NA_character_))
  }
)

test_that(
  "Invalid data is reported, links generated with event IDs provided", 
  {    
    Evt <- read.csv(file.path(offline_files, "TestRedcapAPI_Events.csv"),
                    stringsAsFactors = FALSE)
    Evt$event_id <- rcon$events()$event_id
    
    rcon_off <- 
      expect_warning(
        offlineConnection(
          meta_data = file.path(offline_files, "TestRedcapAPI_DataDictionary.csv"), 
          records = file.path(offline_files, "TestRedcapAPI_Records.csv"), 
          events = Evt,
          project_info = data.frame(project_id = rcon$projectInformation()$project_id, 
                                    is_longitudinal = 1), 
          url = url, 
          version = rcon$version()
        )
      )
    
    Records <- expect_warning(exportRecordsTyped(rcon_off, 
                                                 cast = list(system = as.character)), 
                              "Some records failed validation")
    
    Inv <- reviewInvalidRecords(Records)
    
    expect_character(Inv$link_to_form)
  }
)
    
test_that(
  "Invalid data is reported, links not generated without the version number", 
  {   
    Evt <- read.csv(file.path(offline_files, "TestRedcapAPI_Events.csv"),
                    stringsAsFactors = FALSE)
    Evt$event_id <- rcon$events()$event_id
    
    rcon_off <- 
      expect_warning(
        offlineConnection(
          meta_data = file.path(offline_files, "TestRedcapAPI_DataDictionary.csv"), 
          records = file.path(offline_files, "TestRedcapAPI_Records.csv"), 
          events = Evt,
          version = NULL,
          project_info = data.frame(project_id = rcon$projectInformation()$project_id, 
                                    is_longitudinal = 1), 
          url = url
        )
      )
    
    Records <- expect_warning(exportRecordsTyped(rcon_off, 
                                                 cast = list(system = as.character)), 
                              "Version number not stored")
    
    Inv <- reviewInvalidRecords(Records)
    
    expect_equal(Inv$link_to_form, 
                 c(NA_character_, NA_character_))
  }
)

#####################################################################
# Casting bioportal and sql from offline connections             ####

load(test_path("testdata/test_redcapAPI_MetaData.Rdata"))

MetaData <- test_redcapAPI_MetaData
MetaData <- MetaData[MetaData$field_name %in% c("record_id", "bioportal_test"), ]
MetaData <- rbind(MetaData, MetaData[2, ])
MetaData$field_name[3] <- "sql_test"
MetaData$field_label[3] <- "SQL Field Testing"
MetaData$field_type[3] <- "sql"
MetaData$select_choices_or_calculations[3] <- NA_character_
MetaData$form_name <- rep("this_form", nrow(MetaData))
rownames(MetaData) <- NULL

Records <- data.frame(record_id = 1:3, 
                      bioportal_test = c("a", "a", "b"), 
                      sql_test = c("1", "2", "3"), 
                      this_form_complete = c("0", "1", "2"),
                      stringsAsFactors = FALSE)

ExternalCoding = list(bioportal_test = c(Alpha = "a", 
                                         Bravo = "b"), 
                      sql_test = c(One = "1", 
                                   Two = "2", 
                                   Three = "3"))

test_that(
  "Cast bioportal and sql fields with offline connections",
  {
    roff <- offlineConnection(meta_data = MetaData, 
                              records = Records, 
                              external_coding = ExternalCoding)
    
    Rec <- exportRecordsTyped(roff)
    
    expect_equal(as.character(Rec$bioportal_test), 
                 c("Alpha", "Alpha", "Bravo"))
    
    expect_equal(as.character(Rec$sql_test), 
                 c("One", "Two", "Three"))
  }
)

rm(list = c("MetaData", "Records", "ExternalCoding"))

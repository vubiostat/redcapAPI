context("offlineConnection object")

test_that(
  "offlineConnection creates the right class", 
  {
    expect_class(
      offlineConnection(meta_data = rcon$metadata()),
      classes = c("redcapOfflineConnection", "redcapConnection")
    )
  }
)

#####################################################################
# Functionality - Data from API                                  ####

test_that(
  "Meta Data loads from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_meta_")
    this_data <- rcon$metadata()
    write.csv(this_data, 
              this_file_name, 
              row.names =FALSE)
    
    # From data frame
    roff <- expect_no_error(offlineConnection(meta_data = this_data))
    
    expect_true(roff$has_metadata())
    expect_true(roff$has_fieldnames())
    expect_true(roff$has_instruments())
    
    # From File
    roff <- 
      expect_no_error(
        offlineConnection(meta_data = this_file_name))
    
    unlink(this_file_name) 
    expect_true(roff$has_metadata())
    expect_true(roff$has_fieldnames())
    expect_true(roff$has_instruments())
  }
)

test_that(
  "Arms loads from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_arms_")
    this_data <- rcon$arms()
    write.csv(this_data, 
              this_file_name, 
              row.names =FALSE)
    
    # From data frame
    roff <- expect_no_error(offlineConnection(arms = this_data))
    
    expect_true(roff$has_arms())
    
    # From File
    roff <- 
      expect_no_error(
        offlineConnection(arms = this_file_name))
    unlink(this_file_name) 
    expect_true(roff$has_arms())
  }
)

test_that(
  "Events loads from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_events_")
    this_data <- rcon$events()
    write.csv(this_data, 
              this_file_name, 
              row.names = FALSE)
    
    # From data frame
    roff <- suppressWarnings({ offlineConnection(events = this_data) })
    
    expect_true(roff$has_events())
    
    # From File
    roff <- suppressWarnings({ offlineConnection(events = this_file_name) })
    unlink(this_file_name)
    expect_true(roff$has_events())
  }
)

test_that(
  "Instruments loads from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_instruments_")
    this_data <- rcon$instruments()
    write.csv(this_data, 
              this_file_name, 
              row.names = FALSE)
    
    # From data frame
    roff <- expect_no_error(offlineConnection(instruments = this_data))
    
    expect_true(roff$has_instruments())
    
    # From File
    roff <- 
      expect_no_error(
        offlineConnection(instruments = this_file_name))
    unlink(this_file_name)
    
    expect_true(roff$has_instruments())
  }
)

test_that(
  "Field Names loads from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_fieldnames_")
    this_data <- rcon$fieldnames()
    write.csv(this_data, 
              this_file_name, 
              row.names = FALSE)
    
    # From data frame
    roff <- expect_no_error(offlineConnection(field_names = this_data))
    
    expect_true(roff$has_fieldnames())
    
    # From File
    roff <- 
      expect_no_error(
        offlineConnection(field_names = this_file_name))
    unlink(this_file_name)
    
    expect_true(roff$has_fieldnames())
  }
)

test_that(
  "Mapping loads from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_mapping_")
    this_data <- rcon$mapping()
    write.csv(this_data, 
              this_file_name, 
              row.names = FALSE)
    
    # From data frame
    roff <- expect_no_error(offlineConnection(mapping = this_data))
    
    expect_true(roff$has_mapping())
    
    # From File
    roff <- 
      expect_no_error(
        offlineConnection(mapping = this_file_name))
    
    unlink(this_file_name)
    
    expect_true(roff$has_mapping())
  }
)

test_that(
  "Users loads from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_users_")
    this_data <- rcon$users()
    write.csv(this_data, 
              this_file_name, 
              row.names = FALSE)
    
    # From data frame
    roff <- expect_warning(offlineConnection(users = this_data))
    
    expect_true(roff$has_users())
    
    # From File
    roff <- 
      expect_warning(
        offlineConnection(users = this_file_name))
    unlink(this_file_name)
    
    expect_true(roff$has_users())
  }
)

test_that(
  "Version loads from a string", 
  {
    roff <- expect_no_error(offlineConnection(version = "13.0.0"))
    
    expect_true(roff$has_version())
  }
)

test_that(
  "Project Information loads from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_project_info_")
    this_data <- rcon$projectInformation()
    write.csv(this_data, 
              this_file_name, 
              row.names = FALSE)
    
    # From data frame
    roff <- expect_no_error(offlineConnection(project_info = this_data))
    
    expect_true(roff$has_projectInformation())
    
    # From File
    roff <- 
      expect_no_error(
        offlineConnection(project_info = this_file_name))
    unlink(this_file_name)
    expect_true(roff$has_projectInformation())
  }
)

test_that(
  "File Repository loads from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_file_repo_")
    this_data <- rcon$fileRepository()
    write.csv(this_data, 
              this_file_name, 
              row.names = FALSE)
    
    # From data frame
    roff <- expect_no_error(offlineConnection(file_repo = this_data))
    
    expect_true(roff$has_fileRepository())
    
    # From File
    roff <- 
      expect_no_error(
        offlineConnection(file_repo = this_file_name))
    unlink(this_file_name)
    expect_true(roff$has_fileRepository())
  }
)

test_that(
  "Repeating Instruments and Events loads from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_repeating_instrument_")
    this_data <- rcon$repeatInstrumentEvent()
    write.csv(this_data, 
              this_file_name, 
              row.names = FALSE)
    
    # From data frame
    roff <- expect_no_error(offlineConnection(repeat_instrument = this_data))
    
    expect_true(roff$has_repeatInstrumentEvent())
    
    # From File
    roff <- 
      expect_no_error(
        offlineConnection(repeat_instrument = this_file_name))
    unlink(this_file_name)
    expect_true(roff$has_repeatInstrumentEvent())
  }
)

test_that(
  "Records loads from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_records_")
    this_data <- exportRecordsTyped(rcon, cast = raw_cast)
    write.csv(this_data, 
              this_file_name, 
              row.names = FALSE)
    
    # From data frame
    roff <- expect_no_error(offlineConnection(records = this_data))
    
    expect_true(roff$has_records())
    
    # From File
    roff <- 
      expect_no_error(
        offlineConnection(records = this_file_name))
    unlink(this_file_name)
    
    expect_true(roff$has_records())
  }
)


#####################################################################
# Functionality - Files Downloaded from REDCap UI

filedir <- test_path("testdata", "offlineConnectionFiles")

test_that(
  "Meta Data loads using file from UI", 
  {
    local_reproducible_output(width = 200)
    file <- file.path(filedir, "TestRedcapAPI_DataDictionary.csv")
    
    roff <- expect_no_error(offlineConnection(meta_data = file))
    
    expect_true(roff$has_metadata())
    expect_true(roff$has_fieldnames())
    expect_true(roff$has_instruments())
  }
)

test_that(
  "Arms loads using file from UI", 
  {
    local_reproducible_output(width = 200)
    file <- file.path(filedir, "TestRedcapAPI_Arms.csv")
    
    roff <- expect_no_error(offlineConnection(arms = file))
    
    expect_true(roff$has_arms())
  }
)

test_that(
  "Events loads using file from UI", 
  {
    local_reproducible_output(width = 200)
    file <- file.path(filedir, "TestRedcapAPI_Events.csv")
    
    roff <- expect_warning(offlineConnection(events = file), 
                           "as expected. [{]event_id,")
    
    expect_true(roff$has_events())
  }
)

test_that(
  "Mappings loads using file from UI", 
  {
    local_reproducible_output(width = 200)
    file <- file.path(filedir, "TestRedcapAPI_InstrumentDesignations.csv")
    
    roff <- expect_no_error(offlineConnection(mapping = file))
    
    expect_true(roff$has_mapping())
  }
)

test_that(
  "Users loads using file from UI", 
  {
    local_reproducible_output(width = 200)
    file <- file.path(filedir, "TestRedcapAPI_Users.csv")
    
    roff <- 
      expect_warning(
        offlineConnection(users = file), 
        "as expected. [{]email, firstname, lastname, alerts, mobile_app, mobile_app_download_data[}]")
    
    expect_true(roff$has_users())
  }
)

test_that(
  "Records loads using file from UI", 
  {
    local_reproducible_output(width = 200)
    file <- file.path(filedir, "TestRedcapAPI_Records.csv")
    
    roff <- expect_no_error(offlineConnection(records = file))
    
    expect_true(roff$has_records())
  }
)

#####################################################################
# Argument Validations

test_that(
  "Validate arguments", 
  {
    local_reproducible_output(width = 200)
    expect_error(offlineConnection(meta_data = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(arms = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(events = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(instruments = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(field_names = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(mapping = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(users = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(version = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(project_info = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(file_repo = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(records = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
  }
)


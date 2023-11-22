context("offlineConnection Functionality")

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
  "Repeating instruments loads from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_repeat_instrument_")
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
  "User Roles loads from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_user_roles_")
    this_data <- rcon$user_roles()
    write.csv(this_data, 
              this_file_name, 
              row.names = FALSE)
    
    # From data frame
    roff <- expect_no_error(offlineConnection(user_roles = this_data))
    
    expect_true(roff$has_user_roles())
    
    # From File
    roff <- 
        offlineConnection(user_roles = this_file_name)
    unlink(this_file_name)
    
    expect_true(roff$has_user_roles())
  }
)

test_that(
  "User Role Assignment loads from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_user_roles_assign_")
    this_data <- rcon$user_role_assignment()
    write.csv(this_data, 
              this_file_name, 
              row.names = FALSE)
    
    # From data frame
    roff <- offlineConnection(user_role_assignment = this_data)
    
    expect_true(roff$has_user_role_assignment())
    
    # From File
    roff <- 
        offlineConnection(user_role_assignment = this_file_name)
    unlink(this_file_name)
    
    expect_true(roff$has_user_role_assignment())
  }
)

test_that(
  "DAGs load from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_dags_")
    this_data <- rcon$dags()
    write.csv(this_data, 
              this_file_name, 
              row.names = FALSE)
    
    # From data frame
    roff <- expect_no_error(offlineConnection(dags = this_data))
    
    expect_true(roff$has_dags())
    
    # From File
    roff <- 
        offlineConnection(dags = this_file_name)
    unlink(this_file_name)
    
    expect_true(roff$has_dags())
  }
)

test_that(
  "DAG Assigments load from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_dag_assign_")
    this_data <- rcon$dag_assignment()
    write.csv(this_data, 
              this_file_name, 
              row.names = FALSE)
    
    # From data frame
    roff <- expect_no_error(offlineConnection(dag_assignment = this_data))
    
    expect_true(roff$has_dag_assignment())
    
    # From File
    roff <- 
      offlineConnection(dag_assignment = this_file_name)
    unlink(this_file_name)
    
    expect_true(roff$has_dag_assignment())
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
  "Version loads from a string", 
  {
    roff <- expect_no_error(offlineConnection(version = "13.0.0"))
    
    expect_true(roff$has_version())
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
  "Records loads from data frame and file", 
  {
    this_file_name <- tempfile("test_offline_records_")
    # We haven't put any data in the test project yet. 
    # We can use any data set for the testing at this point.
    this_data <- mtcars
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

test_that(
  "External Codings load from a list", 
  {
    EC <- list(var1 = c(label = "abc"))
    
    roff <- offlineConnection(external_coding = EC)
    
    expect_true(roff$has_externalCoding())
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

# test_that(
#   "Repeating Instruments Information loads using file from UI", 
#   {
#     # There is no file that can be downloaded from the UI
#   }
# )

test_that(
  "Users loads using file from UI", 
  {
    local_reproducible_output(width = 200)
    file <- file.path(filedir, "TestRedcapAPI_Users.csv")
    
    roff <- 
      expect_warning(
        offlineConnection(users = file), 
        "may not operate as expected")
    
    expect_true(roff$has_users())
  }
)

test_that(
  "User Roles loads using file from UI", 
  {
    local_reproducible_output(width = 200)
    file <- file.path(filedir, "TestRedcapAPI_UserRoles.csv")
    
    roff <- 
      expect_warning(
        offlineConnection(user_roles = file))
    
    expect_true(roff$has_user_roles())
  }
)

test_that(
  "User Roles Assignments loads using file from UI", 
  {
    local_reproducible_output(width = 200)
    file <- file.path(filedir, "TestRedcapAPI_UserRoleAssignments.csv")
    
    roff <- 
        expect_warning(offlineConnection(user_role_assignment = file))
    
    expect_true(roff$has_user_role_assignment())
  }
)

test_that(
  "DAGs load using file from UI", 
  {
    local_reproducible_output(width = 200)
    file <- file.path(filedir, "TestRedcapAPI_DAGs.csv")
    
    roff <- 
      offlineConnection(dags = file)
    
    expect_true(roff$has_dags())
  }
)

test_that(
  "DAG Assignments load using file from UI", 
  {
    local_reproducible_output(width = 200)
    file <- file.path(filedir, "TestRedcapAPI_UserDAG.csv")
    
    roff <- 
      offlineConnection(dag_assignment = file)
    
    expect_true(roff$has_dag_assignment())
  }
)

# test_that(
#   "Project Information loads using file from UI", 
#   {
#     # There is no file that can be downloaded from the UI
#   }
# )
# 
# test_that(
#   "Version loads using a file from the UI", 
#   {
#     # There is no file that can be downloaded from the UI
#   }
# )

test_that(
  "Records loads using file from UI", 
  {
    local_reproducible_output(width = 200)
    file <- file.path(filedir, "TestRedcapAPI_Records.csv")
    
    roff <- expect_no_error(offlineConnection(records = file))
    
    expect_true(roff$has_records())
  }
)

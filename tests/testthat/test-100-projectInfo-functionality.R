context("Project Information Methods Functionality")


#####################################################################
# exportProjectInformation functionality

test_that(
  "Export project information", 
  {
    Project <- exportProjectInformation(rcon)
    
    # These constants are defined in redcapDataStructure.R
    expect_true(all(names(Project) %in% c(PROJECT_INFO_FIELDS_EDITABLE, 
                                          PROJECT_INFO_FIELDS_FIXED)))
    
    expect_data_frame(Project, 
                      nrows = 1)
  }
)

#####################################################################
# importProjectInformation functionality

CurrentInfo <- rcon$projectInformation()

test_that(
  "Import new values", 
  {
    NewInfo <- data.frame(project_pi_lastname = "Not Garbett", 
                          display_today_now_button = 0)
    
    n_imported <- importProjectInformation(rcon, 
                                            NewInfo)
    expect_equal(n_imported, "2")
    
    n_imported <- importProjectInformation(rcon, 
                                           NewInfo)
    expect_equal(n_imported, "2")
    
    # cleanup 
    
    n_imported <- importProjectInformation(rcon, 
                                           CurrentInfo)
    expect_equal(n_imported, "18")
  }
)


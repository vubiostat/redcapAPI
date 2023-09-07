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
    
    expect_message(importProjectInformation(rcon, 
                                            NewInfo), 
                   "Fields updated: 2")
    
    
    # cleanup 
    expect_message(importProjectInformation(rcon, 
                                            CurrentInfo), 
                   "Fields updated: 18")
  }
)


context("Export/Import Project Information Functionality")

rcon <- redcapConnection(url = url, 
                         token = SANDBOX_KEY)

#####################################################################
# exportProjectInformation functionality

test_that(
  "Export project information", 
  {
    Project <- exportProjectInformation(rcon)
    
    expect_equal(names(Project), 
                 c("project_id", 
                   "project_title",
                   "creation_time", 
                   "production_time",
                   "in_production", 
                   "project_language", 
                   "purpose", 
                   "purpose_other", 
                   "project_notes", 
                   "custom_record_label",
                   "secondary_unique_field", 
                   "is_longitudinal", 
                   "has_repeating_instruments_or_events", 
                   "surveys_enabled", 
                   "scheduling_enabled", 
                   "record_autonumbering_enabled", 
                   "randomization_enabled", 
                   "ddp_enabled", 
                   "project_irb_number", 
                   "project_grant_number", 
                   "project_pi_firstname", 
                   "project_pi_lastname", 
                   "display_today_now_button", 
                   "missing_data_codes", 
                   "external_modules", 
                   "bypass_branching_erase_field_prompt"))
    
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


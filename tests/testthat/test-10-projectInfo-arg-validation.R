context("export/import ProjectInformation argument validation")

rcon <- redcapConnection(url = url, 
                         token = API_KEY)

#####################################################################
# exportProjectInformation argument validation

test_that(
  "Return an error when rcon is not redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportProjectInformation("not an rcon"), 
                 "no applicable method for 'exportProjectInformation'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportProjectInformation(rcon, 
                                          error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportProjectInformation(rcon, 
                                          config = list(1)), 
                 "'config': Must have names")
    expect_error(exportProjectInformation(rcon, 
                                          config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportProjectInformation(rcon, 
                                          api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportProjectInformation(rcon, 
                                          api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)


#####################################################################
# importProjectInformation argument validation

OldInfo <- rcon$projectInformation()
NewInfo <- data.frame(project_pi_lastname = "Not Garbett")

test_that(
  "Return an error when rcon is not redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(importProjectInformation("not an rcon"), 
                 "no applicable method for 'importProjectInformation'")
  }
)

test_that(
  "Return an error when data is not a data frame", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(importProjectInformation(rcon, 
                                          data = "not a data frame"))
  }
)

test_that(
  "Return an error if refresh is not logical(1)",
  {
    local_reproducible_output(width = 200)
    skip_if(!STRUCTURAL_TEST_READY, 
            "Infrastructure not quite ready for structural tests.")
    
    expect_error(importProjectInformation(rcon, 
                                          data = NewInfo, 
                                          refresh = c(TRUE, FALSE)), 
                 "'refresh': Must have length 1")
    
    expect_error(importProjectInformation(rcon, 
                                          data = NewInfo, 
                                          refresh = "TRUE"), 
                 "'refresh': Must be of type 'logical'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    skip_if(!STRUCTURAL_TEST_READY, 
            "Infrastructure not quite ready for structural tests.")
    
    expect_error(importProjectInformation(rcon, 
                                          data = NewInfo, 
                                          error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(importProjectInformation(rcon, 
                                          data = NewInfo, 
                                          config = list(1)), 
                 "'config': Must have names")
    expect_error(importProjectInformation(rcon, 
                                          data = NewInfo, 
                                          config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(importProjectInformation(rcon, 
                                          data = NewInfo, 
                                          api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(importProjectInformation(rcon, 
                                          data = NewInfo, 
                                          api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

test_that(
  "Enforce the proper value types for each property", 
  {
    skip_if(!STRUCTURAL_TEST_READY, 
            "Infrastructure not quite ready for structural tests.")
    
    NewData <- 
      data.frame(project_title = 123,
                 project_language = 123,
                 purpose = "character",
                 purpose_other = 123,
                 project_notes = 123,
                 custom_record_label = 123,
                 secondary_unique_field = 123,
                 is_longitudinal = "yes",
                 surveys_enabled = "yes",
                 scheduling_enabled = "yes",
                 record_autonumbering_enabled = "yes",
                 randomization_enabled = "yes",
                 project_irb_number = 123,
                 project_grant_number = 123,
                 project_pi_firstname = 123,
                 project_pi_lastname = 123,
                 display_today_now_button = "yes",
                 bypass_branching_erase_field_prompt = "yes")
    
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'project_title': Must be of type 'character'")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'project_language': Must be of type 'character'")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'purpose': Must be of type 'integerish'")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'purpose_other': Must be of type 'character'")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'project_notes': Must be of type 'character'")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'custom_record_label': Must be of type 'character'")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "Variable 'secondary_unique_field': Must be of type 'character'")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'is_longitudinal' must be 0/1")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'surveys_enabled' must be 0/1")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'scheduling_enabled' must be 0/1")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'record_autonumbering_enabled' must be 0/1")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'randomization_enabled' must be 0/1")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'project_irb_number': Must be of type 'character'")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'project_grant_number': Must be of type 'character'")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'project_pi_firstname': Must be of type 'character'")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'project_pi_lastname': Must be of type 'character'")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'display_today_now_button' must be 0/1")
    expect_error(importProjectInformation(rcon, 
                                          data = NewData), 
                 "'bypass_branching_erase_field_prompt' must be 0/1")
  }
)

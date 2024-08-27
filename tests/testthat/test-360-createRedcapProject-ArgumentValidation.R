context("Create REDCap Project Argument Validation")

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    expect_error(createRedcapProject(rcon = "not an rcon", 
                                     project_title = "Just a title"), 
                 "no applicable method for 'createRedcapProject'")
  }
)

test_that(
  "Return an error if the API token is not 64 characters", 
  {
    expect_error(createRedcapProject(rcon = rcon, 
                                     project_title = "Just a title"), 
                 "'rcon[$]token': All elements must have exactly 64 characters")
  }
)

test_that(
  "Return an error if project_title is not character(1)", 
  {
    expect_error(createRedcapProject(rcon = rcon, 
                                     project_title = c("title1", "title2")), 
                 "'project_title': Must have length 1")
    
    expect_error(createRedcapProject(rcon, 
                                     project_title = 123), 
                 "'project_title': Must be of type 'character'")
  }
)

test_that(
  "Return an error if purpose is not one of the approved values", 
  {
    expect_error(createRedcapProject(rcon, 
                                     project_title = "Just a title",
                                     purpose = "something else"), 
                 "'purpose': Must be element of set")
  }
)

test_that(
  "Return an error if purpose_other is not character(1)", 
  {
    expect_error(createRedcapProject(rcon, 
                                     project_title = "Just a title", 
                                     purpose = "Other", 
                                     purpose_other = c("reason1", "reason2")), 
                 "'purpose_other': Must have length 1")
    
    expect_error(createRedcapProject(rcon, 
                                     project_title = "Just a title", 
                                     purpose = "Other", 
                                     purpose_other = 123), 
                 "'purpose_other': Must be of type 'character'")
    
    expect_error(createRedcapProject(rcon, 
                                     project_title = "Just a title", 
                                     purpose = "Other", 
                                     purpose_other = NULL), 
                 "'purpose_other': Must be of type 'character', not 'NULL'")
  }
)

test_that(
  "Return an error if is_longitudinal is not logical(1)", 
  {
    expect_error(createRedcapProject(rcon, 
                                     project_title = "Just a title", 
                                     is_longitudinal = "TRUE"), 
                 "'is_longitudinal': Must be of type 'logical'")
    
    expect_error(createRedcapProject(rcon, 
                                     project_title = "Just a title", 
                                     is_longitudinal = c(TRUE, FALSE)), 
                 "'is_longitudinal': Must have length 1")
  }
)

test_that(
  "Return an error if surveys_enabled is not logical(1)", 
  {
    expect_error(createRedcapProject(rcon, 
                                     project_title = "Just a title", 
                                     surveys_enabled = "TRUE"), 
                 "'surveys_enabled': Must be of type 'logical'")
    
    expect_error(createRedcapProject(rcon, 
                                     project_title = "Just a title", 
                                     surveys_enabled = c(TRUE, FALSE)), 
                 "'surveys_enabled': Must have length 1")
  }
)

test_that(
  "Return an error if record_autonumbering_enabled is not logical(1)", 
  {
    expect_error(createRedcapProject(rcon, 
                                     project_title = "Just a title", 
                                     record_autonumbering_enabled = "TRUE"), 
                 "'record_autonumbering_enabled': Must be of type 'logical'")
    
    expect_error(createRedcapProject(rcon, 
                                     project_title = "Just a title", 
                                     record_autonumbering_enabled = c(TRUE, FALSE)), 
                 "'record_autonumbering_enabled': Must have length 1")
  }
)

test_that(
  "Return an error if xml is not character(1)", 
  {
    expect_error(createRedcapProject(rcon, 
                                     project_title = "Just a title", 
                                     xml = 123), 
                 "'xml': Must be of type 'character'")
    
    expect_error(createRedcapProject(rcon, 
                                     project_title = "Just a title", 
                                     xml = c("xml1", "xml2")), 
                 "'xml': Must have length 1")
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)
    
    # Simulate supertoken
    x <- rcon
    x$token <- paste0(sample(LETTERS, 64, replace=TRUE),collapse='')
  
    expect_error(createRedcapProject(x,
                                     project_title = "Just a title", 
                                     config = list(1)), 
                 "'config': Must have names")
    expect_error(createRedcapProject(x,
                                     project_title = "Just a title", 
                                     config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(createRedcapProject(x,
                                     project_title = "Just a title", 
                                     api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(createRedcapProject(x,
                                     project_title = "Just a title", 
                                     api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

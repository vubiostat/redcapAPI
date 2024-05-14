context("Export Survey Queue Link Argument Validation")

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    expect_error(exportSurveyQueueLink(rcon = "not an rcon", 
                                       record = 1), 
                 "no applicable method for 'exportSurveyQueueLink'")
  }
)

test_that(
  "Return an error if record is not character(1)", 
  {
    expect_error(exportSurveyQueueLink(rcon, 
                                       record = c("1", "2")), 
                 "Variable 'record': Must have length 1")
    
    expect_error(exportSurveyQueueLink(rcon, 
                                       record = c(1, 2)), 
                 "'record': Must have length 1")
    
    expect_error(exportSurveyQueueLink(rcon, 
                                       record = TRUE), 
                 "'record': Must be of type 'character'")
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)

    expect_error(exportSurveyQueueLink(rcon, 
                               record = 1, 
                               config = list(1)), 
                 "'config': Must have names")
    expect_error(exportSurveyQueueLink(rcon, 
                               record = 1, 
                               config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportSurveyQueueLink(rcon, 
                               record = 1, 
                               api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportSurveyQueueLink(rcon, 
                               record = 1, 
                               api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

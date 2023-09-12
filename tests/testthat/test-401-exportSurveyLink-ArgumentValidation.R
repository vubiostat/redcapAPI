context("Export Survey Link Argument Validation")

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    expect_error(exportSurveyLink(rcon = "not an rcon", 
                                  record = 1, 
                                  instrument = "survey"), 
                 "no applicable method for 'exportSurveyLink'")
  }
)

test_that(
  "Return an error if record is not character(1)", 
  {
    expect_error(exportSurveyLink(rcon, 
                                  record = c("1", "2"), 
                                  instrument = "survey"), 
                 "Variable 'record': Must have length 1")
    
    expect_error(exportSurveyLink(rcon, 
                                  record = c(1, 2), 
                                  instrument = "survey"), 
                 "'record': Must have length 1")
    
    expect_error(exportSurveyLink(rcon, 
                                  record = TRUE, 
                                  instrument = "survey"), 
                 "'record': Must be of type 'character'")
  }
)

test_that(
  "Return an error if instrument is not character(1)", 
  {
    expect_error(exportSurveyLink(rcon, 
                                  record = 1, 
                                  instrument = c("survey", "survey2")),
                 "'instrument': Must have length 1")
    
    expect_error(exportSurveyLink(rcon, 
                                  record = 1, 
                                  instrument = 123),
                 "'instrument': Must be of type 'character'")
    
    expect_error(exportSurveyLink(rcon, 
                                  record = 1, 
                                  instrument = "survey"), 
                 "Variable 'instrument': Must be a subset of")
  }
)

test_that(
  "Return an error if event is not character(1)", 
  {
    expect_error(exportSurveyLink(rcon, 
                                  record = 1, 
                                  instrument = "survey", 
                                  event = c("event1", "event2")),
                 "'event': Must have length 1")
    
    expect_error(exportSurveyLink(rcon, 
                                  record = 1, 
                                  instrument = "survey", 
                                  event = 123),
                 "'event': Must be of type 'character'")
    
    expect_error(exportSurveyLink(rcon, 
                                  record = 1, 
                                  instrument = "survey", 
                                  event = "event_k"), 
                 "Variable 'event': Must be a subset of")
  }
)

test_that(
  "Return an error if repeat_instance is not integerish(1)", 
  {
    expect_error(exportSurveyLink(rcon, 
                                  record = 1, 
                                  instrument = "survey", 
                                  repeat_instance = 1:2), 
                 "'repeat_instance': Must have length 1")
    
    expect_error(exportSurveyLink(rcon, 
                                  record = 1, 
                                  instrument = "survey", 
                                  repeat_instance = pi),
                 "'repeat_instance': Must be of type 'integerish'")
    
    expect_error(exportSurveyLink(rcon, 
                                  record = 1, 
                                  instrument = "survey", 
                                  repeat_instance = "1"), 
                 "'repeat_instance': Must be of type 'integerish'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportSurveyLink(rcon, 
                                  record = 1, 
                                  instrument = "survey",
                                  error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportSurveyLink(rcon, 
                                  record = 1, 
                                  instrument = "survey",
                                  config = list(1)), 
                 "'config': Must have names")
    expect_error(exportSurveyLink(rcon, 
                                  record = 1, 
                                  instrument = "survey",
                                  config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportSurveyLink(rcon, 
                                  record = 1, 
                                  instrument = "survey",
                                  api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportSurveyLink(rcon, 
                                  record = 1, 
                                  instrument = "survey",
                                  api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

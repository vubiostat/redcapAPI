context("Export Survey Participants Argument Validation")

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    expect_error(exportSurveyParticipants(rcon = "not an rcon", 
                                          instrument = "survey"), 
                 "no applicable method for 'exportSurveyParticipants'")
  }
)

test_that(
  "Return an error if instrument is not character(1)", 
  {
    expect_error(exportSurveyParticipants(rcon, 
                                          instrument = c("survey", "survey2")),
                 "'instrument': Must have length 1")
    
    expect_error(exportSurveyParticipants(rcon, 
                                          instrument = 123),
                 "'instrument': Must be of type 'character'")
    
    expect_error(exportSurveyParticipants(rcon, 
                                          record = 1, 
                                          instrument = "survey"), 
                 "Variable 'instrument': Must be a subset of")
  }
)

test_that(
  "Return an error if event is not character(1)", 
  {
    expect_error(exportSurveyParticipants(rcon, 
                                          instrument = "survey", 
                                          event = c("event1", "event2")),
                 "'event': Must have length 1")
    
    expect_error(exportSurveyParticipants(rcon, 
                                          instrument = "survey", 
                                          event = 123),
                 "'event': Must be of type 'character'")
    
    expect_error(exportSurveyParticipants(rcon, 
                                          instrument = "survey", 
                                          event = "event_k"), 
                 "Variable 'event': Must be a subset of")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportSurveyParticipants(rcon, 
                                          instrument = "survey",
                                          error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportSurveyParticipants(rcon, 
                                          instrument = "survey",
                                          config = list(1)), 
                 "'config': Must have names")
    expect_error(exportSurveyParticipants(rcon, 
                                          instrument = "survey",
                                          config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportSurveyParticipants(rcon, 
                                          instrument = "survey",
                                          api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportSurveyParticipants(rcon, 
                                          instrument = "survey",
                                          api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

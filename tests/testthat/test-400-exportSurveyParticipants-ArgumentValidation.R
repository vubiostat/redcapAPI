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

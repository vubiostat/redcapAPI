context("Events Methods Argument Validation")


Events <- data.frame(event_name = c("event_1", "event_1", "event_1"), 
                     arm_num = 1:3)

#####################################################################
# Export Events Validation

test_that(
  "Return an error when rcon is not redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportEvents("not an rcon"), 
                 "no applicable method for 'exportEvents'")
  }
)

test_that(
  "Return an error when exportArms is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportEvents(rcon, 
                              arms = c(TRUE, FALSE)), 
                 "'arms': Must be of type 'integerish'")
  }
)

test_that(
  "Return an error if override is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importEvents(rcon, 
                              Events, 
                              override = c(TRUE, FALSE)), 
                 "'override': Must have length 1")
    
    expect_error(importEvents(rcon, 
                              Events, 
                              override = "TRUE"), 
                 "'override': Must be of type 'logical'")
  }
)


#####################################################################
# Import Events Validation


Events <- data.frame(event_name = c("event_1", "event_1", "event_1"), 
                     arm_num = 1:3)

test_that(
  "Return an error when rcon is not redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(importEvents("not an rcon", 
                              data = Events), 
                 "no applicable method for 'importEvents'")
  }
)

test_that(
  "Return an error when data is not a data.frame", 
  {
    local_reproducible_output(width = 500)
    
    EventsImproper <- data.frame(evt_nm = 1:2, 
                                 arm_name = as.character(1:2))
    
    expect_error(importEvents(rcon, 
                              "not a data frame"), 
                 "Variable 'data': Must be of type 'data.frame'")
    
    # Has the correct column names
    expect_error(importEvents(rcon, 
                              data = EventsImproper), 
                 "Variable 'names[(]data[)]': Must be a subset of [{]'event_name','arm_num','unique_event_name','custom_event_label','event_id','days_offset','offset_min','offset_max'[}]")
  }
)

test_that(
  "Return an error if override is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importEvents(rcon, 
                              Events, 
                              override = c(TRUE, FALSE)), 
                 "'override': Must have length 1")
    
    expect_error(importEvents(rcon, 
                              Events, 
                              override = "TRUE"), 
                 "'override': Must be of type 'logical'")
  }
)

test_that(
  "Return an error when error handling isn't one of null, error", 
  {
    local_reproducible_output(width = 200)
   
    expect_error(importEvents(rcon,
                            data = Events,
                            config = list(1)),
                 "'config': Must have names")
    expect_error(importEvents(rcon,
                            data = Events,
                            config = "not a list"),
                 "'config': Must be of type 'list'")
    
    expect_error(importEvents(rcon,
                            data = Events,
                            api_param = list(1)),
                 "'api_param': Must have names")
    expect_error(importEvents(rcon,
                            data = Events,
                            api_param = "not a list"),
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# Delete Events Validation

test_that(
  "Return an error when rcon is not redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(deleteEvents("not an rcon"), 
                 "no applicable method for 'deleteEvents'")
  }
)

test_that(
  "Return an error when events is not character", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(deleteEvents(rcon, 
                            events = c(TRUE, FALSE)), 
                 "'events': Must be of type 'character'")
  }
)


test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(deleteEvents(rcon,
                            events = "event_1_arm_1",
                            config = list(1)), 
                 "'config': Must have names")
    expect_error(deleteEvents(rcon,
                            events = "event_1_arm_1",
                            config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(deleteEvents(rcon, 
                            events = "event_1_arm_1",
                            api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(deleteEvents(rcon, 
                            events = "event_1_arm_1",
                            api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

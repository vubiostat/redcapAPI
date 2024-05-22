context("Events Methods Functionality")

# NOTE: All of the tests for how events behave in classic projects
#       were tested alongside the arms testing. We will not 
#       recreate those tests here. Instead, we will focus only on 
#       behaviors in longitudinal projects.

#####################################################################
# Object to aid in testing                                       ####

Arms <- data.frame(arm_num = 1:3, 
                   name = c("Arm 1", "Arm 2", "Arm 3"), 
                   stringsAsFactors = FALSE)

Arms2 <- data.frame(arm_num = 10:11, 
                    name = c("Arm x", "Arm y"))

Events <- data.frame(event_name = c("event_1", "event_1", "event_1"), 
                     arm_num = 1:3)

Events2 <- data.frame(event_name = c("event_x", "event_y"), 
                      arm_num = 10:11)

#####################################################################
# Functional Testing

test_that(
  "Import, Export, and Deletion of Arms Execute Successfully", 
  {
    local_reproducible_output(width = 200)

    # We start start from an empty classic project. We need to make it
    # longitudinal
    
    n_imported <- importProjectInformation(rcon, 
                                           data.frame(is_longitudinal = 1))
    expect_equal(n_imported, "1")
    
    # Because no arms or events are defined, it still registers as non-longitudinal
    expect_equal(rcon$projectInformation()$is_longitudinal, 
                 0)

    # To be considered 'longitudinal', both arms and events must be defined.
    n_imported <- importArms(rcon, 
                             data = Arms) 
    expect_equal(n_imported, "3")
    
    n_imported <- importEvents(rcon, 
                               data = Events)
    expect_equal(n_imported, "3")
   
    # The project should now be recognized as longitudinal
    expect_equal(rcon$projectInformation()$is_longitudinal, 
                 1)
    
    # These argument validations only trigger for longitudinal
    local_reproducible_output(width = 200)
   
    expect_error(exportArms(rcon, 
                            config = list(1)), 
                 "'config': Must have names")
    expect_error(exportArms(rcon, 
                            config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportArms(rcon, 
                            api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportArms(rcon, 
                            api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
    
    # export the Arms and compare output with what was imported
    expect_data_frame(exportEvents(rcon), 
                      ncols = 5, 
                      nrows = 3)
    
    # delete the Arms
    n_deleted <- deleteEvents(rcon, 
                              events = c("event_1_arm_1", 
                                         "event_1_arm_2", 
                                         "event_1_arm_3"))
    expect_equal(n_deleted, "3")
    
    # backward compatible with event_data argument
    n_imported <- importEvents(rcon, 
                               event_data = Events)
    expect_equal(n_imported, "3")
    
    n_deleted <- deleteEvents(rcon, 
                              events = c("event_1_arm_1", 
                                         "event_1_arm_2", 
                                         "event_1_arm_3"))
    expect_equal(n_deleted, "3")

    # Now the project should no longer be considered longitudinal
    expect_equal(rcon$projectInformation()$is_longitudinal, 
                 0)
  }
)

# This is a test of the override argument in importEvents

test_that(
  "Test the override argument in importEvents",
  {
    local_reproducible_output(width = 200)
    
    # start from an empty project with no arms. It should be recognized as a classical project.
    expect_equal(rcon$projectInformation()$is_longitudinal, 
                 0)
    
    # To be considered 'longitudinal', both arms and events must be defined.
    n_imported <- importArms(rcon, 
                             data = Arms)
    expect_equal(n_imported, "3")
    
    n_imported <- importEvents(rcon, 
                               data = Events)
    expect_equal(n_imported, "3")
    
    expect_data_frame(exportEvents(rcon), 
                      ncols = 5, 
                      nrows = 3)
    
    # we will need to upload events for the new arms too

    OverrideEvent <- data.frame(event_name = c("event_10", 
                                               "event_11", 
                                               "event_12"), 
                                arm_num = 1:3)
    
    n_imported <- importEvents(rcon, 
                               data = OverrideEvent, 
                               override = TRUE)
    expect_equal(n_imported, "3")

    expect_equal(rcon$events()$event_name, 
                 c("event_10", "event_11", "event_12"))
    
    expect_equal(rcon$events()$unique_event_name, 
                 c("event_10_arm_1", "event_11_arm_2", "event_12_arm_3"))
    
    
    
    # Now Clean up from the test
    n_deleted <- deleteArms(rcon, arms = 1:3)
    expect_equal(n_deleted, "3")
  }
)


test_that(
  "Confirm that we can add additional events and delete specific events", 
  {
    local_reproducible_output(width = 200)

    # start from an empty project with no arms. It should be recognized as a classical project.
    expect_equal(rcon$projectInformation()$is_longitudinal, 
                 0)
    
    # To be considered 'longitudinal', both arms and events must be defined.
    n_imported <- importArms(rcon, 
                             data = Arms)
    expect_equal(n_imported, "3")
    
    n_imported <- importEvents(rcon, 
                               data = Events)
    expect_equal(n_imported, "3")
    
    expect_data_frame(exportEvents(rcon), 
                      ncols = 5, 
                      nrows = 3)
    
    # import the additional events
    n_imported <- importArms(rcon, 
                             data = Arms2)
    expect_equal(n_imported, "2")
    
    n_imported <- importEvents(rcon, 
                               data = Events2)
    expect_equal(n_imported, "2")
    
    # Confirm that all of the arms are present.
    expect_data_frame(exportEvents(rcon), 
                      ncols = 5, 
                      nrows = 5)
    
    # Delete only arms 3 and 10
    
    n_deleted <- deleteEvents(rcon, 
                              events = c("event_1_arm_3", 
                                         "event_x_arm_10"))
    expect_equal(n_deleted, "2")
    
    expect_data_frame(exportEvents(rcon), 
                      nrows = 3, 
                      ncols = 5)
    
    # clean up
    n_deleted <- deleteArms(rcon, 
                            arms = c(1, 2, 11))
    expect_equal(n_deleted, "3")
    
    importProjectInformation(rcon, 
                             data.frame(is_longitudinal = 0))
  }
)

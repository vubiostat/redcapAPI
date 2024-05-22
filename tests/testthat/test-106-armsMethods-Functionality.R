context("Arms Methods Functionality")

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
# Classic Project Testing                                        ####

test_that(
  "exportArms returns an empty data frame", 
  {
    local_reproducible_output(width = 200)
    expect_equal(rcon$projectInformation()$is_longitudinal, 
                 0)
    expect_data_frame(exportArms(rcon), 
                      nrows = 0, 
                      ncols = 2)
  }
)

test_that(
  "deleteArms returns a message indicating no arms deleted.", 
  {
    local_reproducible_output(width = 200)
    n_deleted <- deleteArms(rcon, 
                            arms = rcon$arms()$arm_num) 
    expect_equal(n_deleted, "0")
  }
)

# importArms deserves some explanation. Even though we are working on a
# classic project, we can import Arms and Events. But if we try to 
# export them again, the API will kick back an error. The R package
# bypasses the error and returns an empty data frame instead. 
# Let's test that we get back what we intend after importing these.

purgeProject(rcon, 
             arms = TRUE, 
             events = TRUE)

test_that(
  "Import arms into a empty project.", 
  {
    # Import the arms
    n_imported <- importArms(rcon, 
                             data = Arms)
    expect_equal(n_imported, "3")
    
    # backward compatibility with data

    n_imported <- importArms(rcon, 
                             arms_data = Arms)
    expect_equal(n_imported, "3")
    
    expect_equal(rcon$projectInformation()$is_longitudinal, 0)
    
    # Because we aren't yet longitudinal, we don't want arms back yet.
    expect_equal(exportArms(rcon), 
                 REDCAP_ARMS_STRUCTURE)
    
    # Now let's import the events. This should make the project longitudinal
    n_imported <- importEvents(rcon, 
                               data = Events)
    expect_equal(n_imported, "3")
    
    # We haven't allowed the project to use arms/events, so it should 
    # still not be longitudinal, even though we've uploaded arms and events.
    expect_equal(rcon$projectInformation()$is_longitudinal, 
                 0)
    
    # We also shouldn't get back any arms or events on export
    expect_equal(exportArms(rcon), 
                 REDCAP_ARMS_STRUCTURE)
    expect_equal(exportEvents(rcon), 
                 REDCAP_EVENT_STRUCTURE)
    
    # Now let's enable the longitudinal study and make sure we can 
    # get the arms and events out.
    
    n_imported <- importProjectInformation(rcon, 
                                           data.frame(is_longitudinal = 1))
    expect_equal(n_imported, "1")
    
    expect_equal(exportArms(rcon), 
                 Arms)
    expect_equal(exportEvents(rcon)[1:2], # we didn't provide a full specification for Events 
                 Events)
    
    local_reproducible_output(width = 200)
 
    expect_error(deleteArms(rcon,
                            arms = 1,
                            config = list(1)), 
                 "'config': Must have names")
    expect_error(deleteArms(rcon,
                            arms = 1,
                            config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(deleteArms(rcon, 
                            arms = 1,
                            api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(deleteArms(rcon, 
                            arms = 1,
                            api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
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
    # And now we clean up from our testing.
    
    purgeProject(rcon, 
                 arms = TRUE, 
                 events = TRUE)
    
    importProjectInformation(rcon, 
                             data.frame(is_longitudinal = 0))
    
    expect_equal(exportArms(rcon), 
                 REDCAP_ARMS_STRUCTURE)
    expect_equal(exportEvents(rcon), 
                 REDCAP_EVENT_STRUCTURE)
  }
)

#####################################################################
# Longitudinal Project Testing                                   ####
test_that(
  "Import, Export, and Deletion of Arms Execute Successfully", 
  {
    local_reproducible_output(width = 200)
    
    if(rcon$projectInformation()$is_longitudinal == 0){
      importProjectInformation(rcon, 
                               data.frame(is_longitudinal = 1))
    }
    
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
    
    # export the Arms and compare output with what was imported
    expect_equal(exportArms(rcon), 
                 Arms)

    # delete the Arms
    n_deleted <- deleteArms(rcon, 
                            arms = 1:3)
    expect_equal(n_deleted, "3")
    
    # Now the project should no longer be considered longitudinal
    expect_equal(rcon$projectInformation()$is_longitudinal, 0)
  }
)

# This is a test of the override argument in importArms

test_that(
  "Test the override argument in importArms",
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
    
    expect_equal(exportArms(rcon), 
                 Arms)
    
    # we will need to upload events for the new arms too
    n_imported <- importArms(rcon, 
                             data = Arms2, 
                             override = TRUE)
    expect_equal(n_imported, "2")
    
    n_imported <- importEvents(rcon, 
                               data = Events2)
    expect_equal(n_imported, "2")
    
    expect_equal(exportArms(rcon), 
                 Arms2)
    
    # Now Clean up from the test
    n_deleted <- deleteArms(rcon, arms = 10:11)
    expect_equal(n_deleted, "2")
  }
)


test_that(
  "Confirm that we can add additional arms and delete specific arms", 
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
    
    expect_equal(exportArms(rcon), 
                 Arms)
    
    # import the additional arms
    n_imported <- importArms(rcon, 
                             data = Arms2)
    expect_equal(n_imported, "2")
    
    n_imported <- importEvents(rcon, 
                               data = Events2)
    expect_equal(n_imported, "2")

    # Confirm that all of the arms are present.
    expect_equal(exportArms(rcon), 
                 rbind(Arms, Arms2))

    # Delete only arms 3 and 10
    
    n_deleted <- deleteArms(rcon, 
                            arms = c(3, 10))
    expect_equal(n_deleted, "2")
    
    expect_data_frame(exportArms(rcon), 
                      nrows = 3, 
                      ncols = 2)
    
    # clean up
    n_deleted <- deleteArms(rcon, 
                            arms = c(1, 2, 11))
    expect_equal(n_deleted, "3")
    
    importProjectInformation(rcon, 
                             data = data.frame(is_longitudinal = 0))
  }
)

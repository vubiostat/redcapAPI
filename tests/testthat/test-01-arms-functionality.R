context("export/import/delete Arms Functionality")

#####################################################################
# Make the connection and purge the project                      ####

purgeProject(rcon, 
             purge_all = TRUE)

load(test_path("testdata", "RedcapProject_EmptyProject.Rdata"))
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

# We are going to start with an empty/classic project. We want 
# export and delete to respond appropriately when we are starting
# from scratch.
rcon$flush_all()
restoreProject(RedcapProject_EmptyProject, rcon)

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
    expect_message(deleteArms(rcon, 
                              arms = rcon$arms()$arm_num), 
                   "Arms Deleted: None")
    rcon$flush_arms()
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
rcon$flush_all()

test_that(
  "Import arms into a empty project.", 
  {
    # Import the arms
    expect_message(importArms(rcon, 
                              arms_data = Arms), 
                   "Arms imported: 3")
    
    rcon$refresh_projectInformation()
    expect_equal(rcon$projectInformation()$is_longitudinal, 
                 0)
    
    # Because we aren't yet longitudinal, we don't want arms back yet.
    expect_equal(exportArms(rcon), 
                 REDCAP_ARMS_STRUCTURE)
    
    # Now let's import the events. This should make the project longitudinal
    expect_message(importEvents(rcon, 
                                event_data = Events), 
                   "Events imported: 3")
    
    rcon$refresh_projectInformation()
    
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
    
    expect_message(importProjectInformation(rcon, 
                                            data.frame(is_longitudinal = 1)), 
                   "Fields updated: 1")
    
    expect_equal(exportArms(rcon), 
                 Arms)
    expect_equal(exportEvents(rcon)[1:2], # we didn't provide a full specification for Events 
                 Events)
    
    rcon$refresh_arms()
    rcon$refresh_events()
    
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
    expect_message(importArms(rcon, 
                              arms_data = Arms), 
                   "Arms imported: 3")
    
    expect_message(importEvents(rcon, 
                                event_data = Events), 
                   "Events imported: 3")
    
    rcon$refresh_projectInformation()
    
    # The project should now be recognized as longitudinal
    expect_equal(rcon$projectInformation()$is_longitudinal, 
                 1)
    
    # export the Arms and compare output with what was imported
    expect_equal(exportArms(rcon), 
                 Arms)
    
    rcon$refresh_arms()
    
    # delete the Arms
    expect_message(deleteArms(rcon, 
                              arms = 1:3), 
                   "Arms Deleted: 1, 2, 3")
    
    rcon$refresh_projectInformation()
    
    # Now the project should no longer be considered longitudinal
    expect_equal(rcon$projectInformation()$is_longitudinal, 
                 0)
  }
)

# This is a test of the override argument in importArms

test_that(
  "Test the override argument in importArms",
  {
    local_reproducible_output(width = 200)
    
    rcon$refresh_projectInformation()
    # start from an empty project with no arms. It should be recognized as a classical project.
    expect_equal(rcon$projectInformation()$is_longitudinal, 
                 0)
    
    # To be considered 'longitudinal', both arms and events must be defined.
    expect_message(importArms(rcon, 
                              arms_data = Arms), 
                   "Arms imported: 3")
    
    expect_message(importEvents(rcon, 
                                event_data = Events), 
                   "Events imported: 3")
    
    rcon$refresh_projectInformation()
    
    expect_equal(exportArms(rcon), 
                 Arms)
    
    # we will need to upload events for the new arms too
    expect_message(importArms(rcon, 
                              arms_data = Arms2, 
                              override = TRUE), 
                   "Arms imported: 2")
    
    expect_message(importEvents(rcon, 
                                event_data = Events2), 
                   "Events imported: 2")
    
    rcon$refresh_projectInformation()
    
    expect_equal(exportArms(rcon), 
                 Arms2)
    
    
    
    # Now Clean up from the test
    rcon$refresh_arms()
    expect_message(deleteArms(rcon, arms = 10:11), 
                   "Arms Deleted: 10, 11")
  }
)


test_that(
  "Confirm that we can add additional arms and delete specific arms", 
  {
    local_reproducible_output(width = 200)
    
    rcon$refresh_projectInformation()
    # start from an empty project with no arms. It should be recognized as a classical project.
    expect_equal(rcon$projectInformation()$is_longitudinal, 
                 0)
    
    # To be considered 'longitudinal', both arms and events must be defined.
    expect_message(importArms(rcon, 
                              arms_data = Arms), 
                   "Arms imported: 3")
    
    expect_message(importEvents(rcon, 
                                event_data = Events), 
                   "Events imported: 3")
    
    rcon$refresh_projectInformation()
    
    expect_equal(exportArms(rcon), 
                 Arms)
    
    # import the additional arms
    expect_message(importArms(rcon, 
                              arms_data = Arms2), 
                   "Arms imported: 2")
    
    expect_message(importEvents(rcon, 
                                event_data = Events2), 
                   "Events imported: 2")
    
    rcon$refresh_projectInformation()
    
    # Confirm that all of the arms are present.
    expect_equal(exportArms(rcon), 
                 rbind(Arms, Arms2))
    
    rcon$refresh_arms()
    
    # Delete only arms 3 and 10
    
    expect_message(deleteArms(rcon, 
                              arms = c(3, 10)), 
                   "Arms Deleted: 3, 10")
    
    expect_data_frame(exportArms(rcon), 
                      nrows = 3, 
                      ncols = 2)
    
    # clean up
    expect_message(deleteArms(rcon, 
                              arms = c(1, 2, 11)), 
                   "Arms Deleted: 1, 2, 11")
  }
)

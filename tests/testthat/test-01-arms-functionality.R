context("export/import/delete Arms Functionality")

rcon <- redcapConnection(url = url, 
                         token = API_KEY)

#####################################################################
# Arms Functionality

Arms <- data.frame(arm_num = 1:3, 
                   name = c("Arm 1", "Arm 2", "Arm 3"), 
                   stringsAsFactors = FALSE)

Arms2 <- data.frame(arm_num = 10:11, 
                    name = c("Arm x", "Arm y"))

Events <- data.frame(event_name = c("event_1", "event_1", "event_1"), 
                     arm_num = 1:3)

Events2 <- data.frame(event_name = c("event_x", "event_y"), 
                      arm_num = 10:11)

test_that(
  "Import, Export, and Deletion of Arms Execute Successfully", 
  {
    local_reproducible_output(width = 200)
    skip_if(!STRUCTURAL_TEST_READY, 
            "Infrastructure not quite ready for structural tests.")
    
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
                   "Arms 1, 2, 3 deleted.")
    
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
    skip_if(!STRUCTURAL_TEST_READY, 
            "Infrastructure not quite ready for structural tests.")
    
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
    
    expect_equal(exportArms(rcon), 
                 Arms2)
    
    
    
    # Now Clean up from the test
    rcon$refresh_arms()
    expect_message(deleteArms(rcon, arms = 10:11), 
                   "Arms 10, 11 deleted.")
  }
)


test_that(
  "Confirm that we can add additional arms and delete specific arms", 
  {
    local_reproducible_output(width = 200)
    skip_if(!STRUCTURAL_TEST_READY, 
            "Infrastructure not quite ready for structural tests.")
    
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
    
    # Confirm that all of the arms are present.
    expect_equal(exportArms(rcon), 
                 rbind(Arms, Arms2))
    
    rcon$refresh_arms()
    
    # Delete only arms 3 and 10
    
    expect_message(deleteArms(rcon, 
                              arms = c(3, 10)), 
                   "Arms 3, 10 deleted")
    
    expect_data_frame(exportArms(rcon), 
                      nrows = 3, 
                      ncols = 2)
    
    # clean up
    expect_message(deleteArms(rcon, 
                              arms = c(1, 2, 11)), 
                   "Arms 1, 2, 11 deleted.")
  }
)

context("Export Typed Records with Events Funtionality")

# NOTE: Data for these tests was established in 
#       test-200-exportTypedRecords-Functionality.R

Mappings <- exportMappings(rcon)

#####################################################################
# Modify data for testing                                        ####

Mappings2 <- Mappings
Mappings2$arm_num <- 2
Mappings2$unique_event_name <- "event_2_arm_2"

importMappings(rcon, 
               data = rbind(Mappings, Mappings2))
                  

importRecords(rcon, 
              data = data.frame(record_id = 1, 
                                redcap_event_name = "event_2_arm_2"))

test_that(
  "Data returned only for designated event",
  {
    Records <- exportRecordsTyped(rcon, events = "event_1_arm_1", 
                                  cast = list(system = castRaw))
    expect_true(all(Records$redcap_event_name %in% "event_1_arm_1"))
  }
)

test_that(
  "Event names can be labelled or raw", 
  {
    Records <- exportRecordsTyped(rcon)
    expect_equal(levels(Records$redcap_event_name),
                 c("Event 1 (Arm 1: Arm 1)", "Event 2 (Arm 2: Arm2)"))
    
    Records <- exportRecordsTyped(rcon, 
                                  cast = list(system = castRaw))
    expect_equal(unique(Records$redcap_event_name), 
                 c("event_1_arm_1", "event_2_arm_2"))
  }
)

#####################################################################
# Cleanup the data

importRecords(rcon, 
              data = data.frame(record_id = 1, 
                                redcap_event_name = "event_1_arm_1"))
importMappings(rcon, 
               data = Mappings)

context("Export Typed Records with Repeating Events and Instruments Functionality")

# FIXME: Add tests for this function



# #####################################################################
# # Avoid error on System Fields (Issue #102)                      ####
# # These tests are placed here because by this point we have all
# # of the system fields included.
# test_that(
#   "Including system fields in 'fields' doesn't produce an error", 
#   {
#     # FIXME: This test would be better run on a project that has
#     #        repeating instruments and events, and possibly DAGs
#     # Four use cases from #102
#     
#     # 1. User requests no fields (fields = NULL) return all fields
#     #    This is covered in other tests.
#     
#     # 2. User requests only actual fields (no system fields in 'fields')
#     #    Return actual fields + system fields
#     
#     Rec <- exportRecordsTyped(rcon, 
#                               fields = "record_id")
#     expect_true("redcap_event_name" %in% names(Rec))
#     
#     # 3. User requests actual fields + system fields. 
#     #    Return only the requested fields
#     # FIXME: This test would be better if it had more system fields 
#     #        available in the project
#     
#     Rec <- exportRecordsTyped(rcon, 
#                               fields = c("record_id", "redcap_event_name"))
#     expect_true(all(c("record_id", "redcap_event_name") %in% names(Rec)))
#     # 4. User requests only system fields
#     #    Return only system fields
#     
#     Rec <- exportRecordsTyped(rcon, 
#                               fields = c("redcap_event_name"))
#     expect_true(names(Rec) == "redcap_event_name")
#   }
# )
# 
# 
# #####################################################################
# # Always include ID fields                                       ####
# 
# test_that(
#   "ID fields are included on all calls", 
#   {
#     minimum_field <- c("record_id", 
#                        "redcap_event_name", 
#                        "redcap_repeat_instrument", 
#                        "redcap_repeat_instance", 
#                        "redcap_data_access_group")
#     
#     # ID field and system fields when just the ID field is requested
#     
#     Rec <- exportRecordsTyped(rcon, 
#                               fields = "record_id")
#     expect_equal(names(Rec), 
#                  minimum_field)
#     
#     # ID field and system fields when a single form is requested
#     
#     Rec <- exportRecordsTyped(rcon, 
#                               forms = c("randomization"))
#     expect_true(all(minimum_field %in% names(Rec)))
#     
#     # Now let's make a secondary unique field
#     NewInfo <- data.frame(secondary_unique_field = "text_test")
#     importProjectInformation(rcon, NewInfo)
#     rcon$refresh_projectInformation()
#     
#     Rec <- exportRecordsTyped(rcon, 
#                               forms = c("randomization"))
#     expect_true(all(c(minimum_field, "text_test") %in% names(Rec)))
#     
#     NewInfo <- data.frame(secondary_unique_field = "", 
#                           surveys_enabled = 0)
#     importProjectInformation(rcon, NewInfo)
#     rcon$refresh_projectInformation()
#   }
# )
# 



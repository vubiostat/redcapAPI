context("Preserve, Purge, and Restore Projects Functionality")

#####################################################################
# Preserve, Purge,  and Restore a Project                        ####

test_that(
  "Preserve a project", 
  {
    ProjectList <- preserveProject(rcon)
    
    expect_list(ProjectList, 
                names = "named")
    
    expect_subset(names(ProjectList), 
                  choices = c("project_information", 
                              "arms", 
                              "events", 
                              "meta_data", 
                              "mappings", 
                              "repeating_instruments", 
                              "users", 
                              "user_roles", 
                              "user_role_assignments", 
                              "dags", 
                              "dag_assignments", 
                              "records"))
  }
)

# test_that(
#   "Purge and restore a project from data frames",
#   {
#     expect_no_error(purgeProject(rcon, records = TRUE))
# 
#     expect_no_error(restoreProject(rcon,
#                                    project_information = rcon$project_information(),
#                                    meta_data = rcon$meta_data()))
#   }
# )

# test_that(
#   "Purge and restore a project from a list",
#   {
#     expect_no_error(purgeProject(rcon, records = TRUE))
#     
#     expect_no_error(restoreProject(TestingBranchingLogic, 
#                                    rcon = rcon))
#   }
# )
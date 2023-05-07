context("Purge and Restore Projects")

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

#####################################################################
# Preserve Project Argument Validation                           ####

test_that(
  "preserveProject argument validations", 
  {
    local_reproducible_output(width = 200)
    expect_error(preserveProject("not rcon"), 
                 "no applicable method for 'preserveProject'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(preserveProject(rcon, 
                                 error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(preserveProject(rcon, 
                                 config = list(1)), 
                 "'config': Must have names")
    expect_error(preserveProject(rcon, 
                                 config = "not a list"), 
                 "'config': Must be of type 'list'")
  }
)

#####################################################################
# Purge Project Argument Validation                              ####

test_that(
  "Purge arguments are logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(purgeProject("not rcon"), 
                 "no applicable method for 'purgeProject'")
    
    # Arms 
    expect_error(purgeProject(rcon, 
                              arms = "FALSE"), 
                 "'arms': Must be of type 'logical'")
    expect_error(purgeProject(rcon, 
                              arms = c(FALSE, TRUE)), 
                 "'arms': Must have length 1")
    
    # Events
    expect_error(purgeProject(rcon, 
                              events = "FALSE"), 
                 "'events': Must be of type 'logical'")
    expect_error(purgeProject(rcon, 
                              events = c(FALSE, TRUE)), 
                 "'events': Must have length 1")
    
    # Users
    expect_error(purgeProject(rcon, 
                              users = "FALSE"), 
                 "'users': Must be of type 'logical'")
    expect_error(purgeProject(rcon, 
                              users = c(FALSE, TRUE)), 
                 "'users': Must have length 1")
    
    # User Roles
    expect_error(purgeProject(rcon, 
                              user_roles = "FALSE"), 
                 "'user_roles': Must be of type 'logical'")
    expect_error(purgeProject(rcon, 
                              user_roles = c(FALSE, TRUE)), 
                 "'user_roles': Must have length 1")
    
    # DAGs
    expect_error(purgeProject(rcon, 
                              dags = "FALSE"), 
                 "'dags': Must be of type 'logical'")
    expect_error(purgeProject(rcon, 
                              dags = c(FALSE, TRUE)), 
                 "'dags': Must have length 1")
  
    # Records
    expect_error(purgeProject(rcon, 
                              records = "FALSE"), 
                 "'records': Must be of type 'logical'")
    expect_error(purgeProject(rcon, 
                              records = c(FALSE, TRUE)), 
                 "'records': Must have length 1")  
    
    # purge_all
    expect_error(purgeProject(rcon, 
                              purge_all = "FALSE"), 
                 "'purge_all': Must be of type 'logical'")
    expect_error(purgeProject(rcon, 
                              purge_all = c(FALSE, TRUE)), 
                 "'purge_all': Must have length 1") 
    
    # flush
    expect_error(purgeProject(rcon, 
                              flush = "FALSE"), 
                 "'flush': Must be of type 'logical'")
    expect_error(purgeProject(rcon, 
                              flush = c(FALSE, TRUE)), 
                 "'flush': Must have length 1") 
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(purgeProject(rcon, 
                              error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(purgeProject(rcon, 
                              config = list(1)), 
                 "'config': Must have names")
    expect_error(purgeProject(rcon, 
                              config = "not a list"), 
                 "'config': Must be of type 'list'")
  }
)

#####################################################################
# Restore Project Argument Validation                            ####

test_that(
  "restoreProject Argument Validation", 
  {
    local_reproducible_output(width = 200)
    expect_error(restoreProject("not rcon"), 
                 "no applicable method for 'restoreProject'")
    
    expect_error(restoreProject(rcon, 
                                project_information = "project_information"), 
                 "'project_information': Must be of type 'data.frame'")
    
    expect_error(restoreProject(rcon, 
                                arms = "arms"), 
                 "'arms': Must be of type 'data.frame'")
    
    expect_error(restoreProject(rcon, 
                                events = "events"), 
                 "'events': Must be of type 'data.frame'")
    
    expect_error(restoreProject(rcon, 
                                meta_data = "meta_data"), 
                 "'meta_data': Must be of type 'data.frame'")
    
    expect_error(restoreProject(rcon, 
                                mappings = "mappings"), 
                 "'mappings': Must be of type 'data.frame'")
    
    expect_error(restoreProject(rcon, 
                                repeating_instruments = "repeating_instruments"), 
                 "'repeating_instruments': Must be of type 'data.frame'")
    
    expect_error(restoreProject(rcon, 
                                users = "users"), 
                 "'users': Must be of type 'data.frame'")
    
    expect_error(restoreProject(rcon, 
                                user_roles = "user_roles"), 
                 "'user_roles': Must be of type 'data.frame'")
    
    expect_error(restoreProject(rcon, 
                                user_role_assignments = "user_role_assignments"), 
                 "'user_role_assignments': Must be of type 'data.frame'")
    
    expect_error(restoreProject(rcon, 
                                dags = "dags"), 
                 "'dags': Must be of type 'data.frame'")
    
    expect_error(restoreProject(rcon, 
                                dag_assignments = "dag_assignments"), 
                 "'dag_assignments': Must be of type 'data.frame'")
    
    expect_error(restoreProject(rcon, 
                                records = "records"), 
                 "'records': Must be of type 'data.frame'")
    
    # flush
    expect_error(purgeProject(rcon, 
                              flush = "FALSE"), 
                 "'flush': Must be of type 'logical'")
    expect_error(purgeProject(rcon, 
                              flush = c(FALSE, TRUE)), 
                 "'flush': Must have length 1") 
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(restoreProject(rcon, 
                                error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(restoreProject(rcon, 
                                config = list(1)), 
                 "'config': Must have names")
    expect_error(restoreProject(rcon, 
                                config = "not a list"), 
                 "'config': Must be of type 'list'")

  }
)

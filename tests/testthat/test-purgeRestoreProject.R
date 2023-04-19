context("Purge and Restore Projects")

rcon <- redcapConnection(url = url, 
                         token = API_KEY)

#####################################################################
# Purge and Restore a Project                                    ####

# I'm not sure I'm ready to implement this test without putting more 
# thought into what project we want to restore for testing.

# test_that(
#   "Purge and restore a project", 
#   {
#     expect_no_error(purgeProject(rcon, records = TRUE))
#     
#     expect_no_error(restoreProject(rcon, 
#                                    project_information = TestingBranchingLogic$ProjectInformation, 
#                                    meta_data = TestingBranchingLogic$MetaData,
#                                    records = TestingBranchingLogic$Records))
#   }
# )

#####################################################################
# Purge Project Argument Validation                              ####

test_that(
  "Purge arguments are logical(1)", 
  {
    expect_error(purgeProject("not rcon"), 
                 "'rcon': Must inherit from class 'redcapApiConnection'")
    
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
    
    expect_error(purgeProject(rcon, 
                              api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(purgeProject(rcon, 
                              api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# Restore Project Argument Validation                            ####

test_that(
  "restoreProject Argument Validation", 
  {
    expect_error(restoreProject("not rcon"), 
                 "'rcon': Must inherit from class 'redcapApiConnection'")
    
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
    
    expect_error(restoreProject(rcon, 
                                api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(restoreProject(rcon, 
                                api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

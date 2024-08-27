context("Data Access Group Assignment Methods Argument Validation")

#####################################################################
# exportUserDagAssignments Argument Validation                   ####

test_that(
  "Return an error is rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportUserDagAssignments("not an rcon"), 
                 "no applicable method for 'exportUserDagAssignments'")
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)
   expect_error(exportUserDagAssignments(rcon, 
                                          config = list(1)), 
                 "'config': Must have names")
    expect_error(exportUserDagAssignments(rcon, 
                                          config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportUserDagAssignments(rcon, 
                                          api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportUserDagAssignments(rcon, 
                                          api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)


#####################################################################
# importUserDagAssignments Argument Validation                   ####

test_that(
  "Return an error is rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(importUserDagAssignments("not an rcon",
                                          data = REDCAP_DAG_ASSIGNMENT_STRUCTURE),
                 "no applicable method for 'importUserDagAssignments'")
  }
)

test_that(
  "Return an error if data is not  data frame", 
  {
    local_reproducible_output(width = 200)
    expect_error(importUserDagAssignments(rcon, 
                                          "not data"), 
                 "'data': Must be of type 'data.frame'")
  }
)

test_that(
  "Return an error if data doesn't have the right structure", 
  {
    local_reproducible_output(width = 200)
    BadData <- data.frame(bad_var = "username", 
                          not_dag = "dag name")
    
    expect_error(importUserDagAssignments(rcon, 
                                          data = BadData), 
                 "'names[(]data[)]': Must be a subset of")
    
    BadData <- data.frame(username = "username", 
                          redcap_data_access_group = "dag_name")
    
    expect_error(importUserDagAssignments(rcon, 
                                          data = BadData), 
                 "'data[$]username': Must be a subset of")
    
    expect_error(importUserDagAssignments(rcon, 
                                          data = BadData), 
                 "'data[$]redcap_data_access_group': Must be a subset of")
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)
   
    expect_error(importUserDagAssignments(rcon,
                                          data = REDCAP_DAG_ASSIGNMENT_STRUCTURE, 
                                          config = list(1)), 
                 "'config': Must have names")
    expect_error(importUserDagAssignments(rcon,
                                          data = REDCAP_DAG_ASSIGNMENT_STRUCTURE, 
                                          config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(importUserDagAssignments(rcon,
                                          data = REDCAP_DAG_ASSIGNMENT_STRUCTURE, 
                                          api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(importUserDagAssignments(rcon,
                                          data = REDCAP_DAG_ASSIGNMENT_STRUCTURE, 
                                          api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

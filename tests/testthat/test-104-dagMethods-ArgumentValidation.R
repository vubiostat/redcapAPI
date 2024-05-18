context("Data Access Group Methods Argument Validation")

#####################################################################
# exportDags Argument Validation                                 ####

test_that(
  "Return an error is rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportDags("not an rcon"), 
                 "no applicable method for 'exportDags'")
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)

    expect_error(exportDags(rcon, 
                            config = list(1)), 
                 "'config': Must have names")
    expect_error(exportDags(rcon, 
                            config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportDags(rcon, 
                            api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportDags(rcon, 
                            api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# importDags Argument Validation                                 ####

test_that(
  "Return an error is rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(importDags("not an rcon", 
                            data = REDCAP_DAG_STRUCTURE), 
                 "no applicable method for 'importDags'")
  }
)

test_that(
  "Return an error if data is not a data.frame",
  {
    local_reproducible_output(width = 200)
    expect_error(importDags(rcon, 
                            data = "not a data frame"), 
                 "'data': Must be of type 'data.frame'")
    
    expect_error(importDags(rcon, 
                            data = data.frame(bad_var = "something")), 
                 "'names[(]data[)]': Must be a subset of")
  }
)

test_that(
  "Return an error if a unique_group_name isn't in the project", 
  {
    local_reproducible_output(width = 200)
    FailDag <- data.frame(data_access_group_name = "Temp DAG", 
                          unique_group_name = "bad name")
    expect_error(importDags(rcon, 
                            data = FailDag), 
                 "'data[$]unique_group_name': Must be a subset of")
  }
)


test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)
   
    expect_error(importDags(rcon, 
                            data = REDCAP_DAG_STRUCTURE, 
                            config = list(1)), 
                 "'config': Must have names")
    expect_error(importDags(rcon, 
                            data = REDCAP_DAG_STRUCTURE, 
                            config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(importDags(rcon, 
                            data = REDCAP_DAG_STRUCTURE, 
                            api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(importDags(rcon, 
                            data = REDCAP_DAG_STRUCTURE, 
                            api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# deleteDags Argument Validation                                 ####

test_that(
  "Return an error is rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteDags("not an rcon", 
                            dags = "dag_name"), 
                 "no applicable method for 'deleteDags'")
  }
)

test_that(
  "Return an error if dags is not a character", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteDags(rcon, 
                            dags = 123), 
                 "'dags': Must be of type 'character'")
  }
)

test_that(
  "Return an error if dags is not an existing DAG", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteDags(rcon, 
                            dags = "dag_name"), 
                 "'dags': Must be a subset of")
  }
)


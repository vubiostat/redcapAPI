context("Switch DAGs Argument Validation")

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(switchDag("not an rcon", 
                           "test_dag_1"), 
                 "no applicable method for 'switchDag'")
  }
)

test_that(
  "Return an error if dag is not character(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(switchDag(rcon, 
                           c("test_dag_1", "test_dag_2")),
                 "'dag': Must have length 1")
    expect_error(switchDag(rcon, 
                           123), 
                 "'dag': Must be of type 'character'")
  }
)

test_that(
  "Return an error if dag is not in the subset of DAGs in the project", 
  {
    local_reproducible_output(width = 200)
    expect_error(switchDag(rcon, 
                           dag = "Garfield likes Lasagna"), 
                 "'dag': Must be a subset of")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(switchDag(rcon, 
                           dag = "test_dag_1",
                           error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(switchDag(rcon, 
                           dag = "test_dag_1",
                           config = list(1)), 
                 "'config': Must have names")
    expect_error(switchDag(rcon, 
                           dag = "test_dag_1",
                           config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(switchDag(rcon, 
                           dag = "test_dag_1",
                           api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(switchDag(rcon, 
                           dag = "test_dag_1",
                           api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

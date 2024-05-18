context("Data Access Group Methods Functionality")

test_that(
  "Import, Export, Delete Data Access Groups", 
  {
    NewDag <- data.frame(data_access_group_name = c("Testing DAG 1", "Testing DAG 2"),
                         unique_group_name = rep(NA_character_, 2))
    
    n_imported <- importDags(rcon, 
                             data = NewDag)
    expect_equal(n_imported, "2")
    
    StoredDag <- exportDags(rcon)
    
    expect_data_frame(StoredDag, 
                      ncol = 3, 
                      nrows = 2)
    
    expect_equal(StoredDag, 
                 rcon$dags())
    
    ChangeDag <- StoredDag[1, ]
    ChangeDag$data_access_group_name <- "A different name"
    
    n_imported <- importDags(rcon, 
                             data = ChangeDag)
    expect_equal(n_imported, "1")
    
    expect_true("A different name" %in% rcon$dags()$data_access_group_name)
    expect_true("a_different_name" %in% rcon$dags()$unique_group_name)
    
    local_reproducible_output(width = 200)
   
    # Refactor of makeApiCall made these tests need and actual dag exist,
    # so these argument validation tests have moved here.
    expect_error(deleteDags(rcon, 
                            dags = "testing_dag_2",
                            config = list(1)), 
                 "'config': Must have names")
    expect_error(deleteDags(rcon, 
                            dags = "testing_dag_2",
                            config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(deleteDags(rcon, 
                            dags = "testing_dag_2", 
                            api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(deleteDags(rcon, 
                            dags = "testing_dag_2", 
                            api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
    
    deleted_groups <- rcon$dags()$unique_group_name
    n_deleted <- deleteDags(rcon, 
                            deleted_groups)
    expect_equal(n_deleted, 
                 as.character(length(deleted_groups))) 
  }
)

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
    
    deleted_groups <- rcon$dags()$unique_group_name
    n_deleted <- deleteDags(rcon, 
                            deleted_groups)
    expect_equal(n_deleted, 
                 as.character(length(deleted_groups))) 
  }
)

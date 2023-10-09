context("Data Access Group Assignment Methods Functionality")

test_that(
  "Import and Export DAG Assignments", 
  {
    skip_if(!RUN_USER_TESTS, 
            "DAG Assignment tests without an expendable user could have negative consequences and are not run")
    # we don't really want to run these until we have an expendable user. 
    # otherwise we run the risk of cutting off access a user running the
    # test suite would need to continue testing.
    
    this_dag <- "Temporary DAG"
    
    # Create a Data Access Group for testing
    AddDag <- data.frame(data_access_group_name = this_dag, 
                         unique_group_name = NA_character_, 
                         stringsAsFactors = FALSE)
    
    importDags(rcon, 
               data = AddDag)
    
    # Add a user to the group
    AddDagAssign <- data.frame(username = EXPENDABLE_USER, 
                               redcap_data_access_group = rcon$dags()$unique_group_name, 
                               stringsAsFactors = FALSE)
    
    n_imported <- importUserDagAssignments(rcon, 
                                           data = AddDagAssign)
    expect_equal(n_imported, "1")
    
    # Get the assignments and check the values
    CurrentDag <- exportUserDagAssignments(rcon)
    
    this_dag_setting <- CurrentDag$redcap_data_access_group[CurrentDag$username == EXPENDABLE_USER]
    
    expect_data_frame(CurrentDag, 
                      ncols = 2)
    expect_equal(this_dag_setting, 
                 "temporary_dag")
    expect_equal(this_dag_setting, 
                 gsub("([[:punct:]]|\\s)", "_", tolower(this_dag)))
    
    
    # Unassign the user from the DAG
    UnassignDag <- data.frame(username = EXPENDABLE_USER, 
                              redcap_data_access_group = NA_character_, 
                              stringsAsFactors = FALSE)
    
    n_imported <- importUserDagAssignments(rcon, 
                                           data = UnassignDag)
    expect_equal(n_imported, "1")
    
    deleteDags(rcon, 
               dags = rcon$dags()$unique_group_name)
    
  }
)

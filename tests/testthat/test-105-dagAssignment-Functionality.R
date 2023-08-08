context("Data Access Group Assignment Methods Functionality")

test_that(
  "Import and Export DAG Assignments", 
  {
    skip_if(length(EXPENDABLE_USER) == 0, 
            "DAG Assignment tests without an expendable user could have negative consequences and are not run")
    # we don't really want to run these until we have an expendable user. 
    # otherwise we run the risk of cutting off access a user running the
    # test suite would need to continue testing.
    
    this_dag <- EXPENDABLE_USER
    this_user <- rcon$users()$username[1]
    
    # Create a Data Access Group for testing
    AddDag <- data.frame(data_access_group_name = this_dag, 
                         unique_group_name = NA_character_, 
                         stringsAsFactors = FALSE)
    
    importDags(rcon, 
               data = AddDag)
    
    # Add a user to the group
    AddDagAssign <- data.frame(username = this_user, 
                               redcap_data_access_group = rcon$dags()$unique_group_name, 
                               stringsAsFactors = FALSE)
    
    expect_message(importUserDagAssignments(rcon, 
                                            data = AddDagAssign), 
                   "User-DAG Assignments Added/Modified: 1")
    
    # Get the assignments and check the values
    CurrentDag <- exportUserDagAssignments(rcon)
    
    expect_data_frame(CurrentDag, 
                      ncols = 2, 
                      nrow = 1)
    expect_equal(CurrentDag$username, 
                 this_user)
    expect_equal(CurrentDag$redcap_data_access_group, 
                 gsub("([[:punct:]]|\\s)", "_", tolower(this_dag)))
    
    
    # Unassign the user from the DAG
    UnassignDag <- data.frame(username = this_user, 
                              redcap_data_access_group = NA_character_, 
                              stringsAsFactors = FALSE)
    
    expect_message(importUserDagAssignments(rcon, 
                                            data = UnassignDag), 
                   "User-DAG Assignments Added/Modified: 1")
    
    deleteDags(rcon, 
               dags = rcon$dags()$unique_group_name)
    
  }
)
context("Export Records Functionality with Data Access Groups")

# NOTE: Data for these tests was established in 
#       test-200-exportTypedRecords-Functionality.R

ImportData <- exportRecordsTyped(rcon, 
                                 cast = raw_cast)
ImportData <- castForImport(ImportData, 
                            rcon)

#####################################################################
# Create DAGs to use in testing                                  ####

importDags(rcon, 
           data = data.frame(data_access_group_name = c("Test DAG 1", 
                                                        "Test DAG 2"), 
                             unique_group_name = rep(NA_character_, 2)))

ImportData$redcap_data_access_group <- rep(rcon$dags()$unique_group_name, 
                                           length.out = nrow(ImportData))

importRecords(rcon, ImportData)

#####################################################################
# Export Data Access Groups

test_that(
  "DAGs can be labelled or raw", 
  {
    Dags <- exportRecords(rcon, 
                          export_dags = TRUE)
    expect_true(all(Dags$redcap_data_access_group %in% c("test_dag_1", "test_dag_2")))
    
    DagsLabel <- exportRecords(rcon, 
                            export_dags = TRUE, 
                            raw_or_label = "label")  
    expect_true(all(DagsLabel$redcap_data_access_group %in% c("Test DAG 1", "Test DAG 2")))
  }
)

test_that(
  "DAGs can be excluded", 
  {
    Rec <- exportRecords(rcon, export_dags = FALSE)
    expect_false("redcap_data_access_group" %in% names(Rec))
  }
)

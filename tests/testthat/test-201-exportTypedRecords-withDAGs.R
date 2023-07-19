context("Export Typed Records with DAGs Functionality")

# NOTE: Data for these tests was established in 
#       test-200-exportTypedRecords-Functionality.R

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
    DagLabelled <- exportRecordsTyped(rcon, 
                                      dags = TRUE)
    expect_equal(levels(DagLabelled$redcap_data_access_group), 
                 c("Test DAG 1", "Test DAG 2"))
  
    DagRaw <- exportRecordsTyped(rcon, 
                                 dags = TRUE, 
                                 cast = list(system = castRaw))  
    expect_equal(unique(DagRaw$redcap_data_access_group), 
                 c("test_dag_1", "test_dag_2"))
  }
)


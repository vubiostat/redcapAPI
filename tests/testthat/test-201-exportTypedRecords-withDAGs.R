context("Export Typed Records with DAGs Functionality")

# NOTE: Data for these tests was established in 
#       test-200-exportTypedRecords-Functionality.R

ImportData <- exportRecordsTyped(rcon, 
                                 cast = raw_cast)
ImportData <- castForImport(ImportData, 
                            rcon, 
                            cast = list(number_1dp = as.numeric, 
                                        number_2dp = as.numeric, 
                                        number_1dp_comma_decimal = as.numeric, 
                                        number_2dp_comma_decimal = as.numeric))

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
                                      dag = TRUE)
    expect_equal(levels(DagLabelled$redcap_data_access_group), 
                 c("Test DAG 1", "Test DAG 2"))
  
    DagRaw <- exportRecordsTyped(rcon, 
                                 dag = TRUE, 
                                 cast = list(system = castRaw))  
    expect_equal(unique(DagRaw$redcap_data_access_group), 
                 c("test_dag_1", "test_dag_2"))
  }
)

test_that(
  "DAGs can be excluded", 
  {
    Rec <- exportRecordsTyped(rcon, dag = FALSE)
    expect_false("redcap_data_access_group" %in% names(Rec))
  }
)

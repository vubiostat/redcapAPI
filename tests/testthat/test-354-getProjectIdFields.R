context("Get Project ID Fields")

test_that(
  "Argument Validation", 
  {
    local_reproducible_output(width = 200)
    expect_error(getProjectIdFields("not an rcon"), 
                 "'rcon': Must inherit from class 'redcapConnection'")
  }
)

test_that(
  "Get the ID fields", 
  {
    expect_equal(getProjectIdFields(rcon), 
                 "record_id")
    
    # Force a secondary unique field
    NewInfo <- data.frame(secondary_unique_field = "text_test")
    importProjectInformation(rcon, NewInfo)
    
    expect_equal(getProjectIdFields(rcon), 
                 c("record_id", "text_test"))
    
    # Cleanup
    NewInfo <- data.frame(secondary_unique_field = "", 
                          surveys_enabled = 0)
    importProjectInformation(rcon, NewInfo)
  }
)

test_that("Does not return secondary field that is not in current fields",
  {
    # Force a secondary unique field
    NewInfo <- data.frame(secondary_unique_field = "does_not_exist")
    importProjectInformation(rcon, NewInfo)
    
    expect_warning(ids <- getProjectIdFields(rcon), "secondary unique field that does not exist")
    expect_equal(ids, "record_id")
    
    # Cleanup
    NewInfo <- data.frame(secondary_unique_field = "", 
                          surveys_enabled = 0)
    importProjectInformation(rcon, NewInfo)
  }
)

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
    rcon2 <- rcon
    NewInfo <- data.frame(secondary_unique_field = "text_test")
    importProjectInformation(rcon2, NewInfo)
    rcon2$refresh_projectInformation()
    
    expect_equal(getProjectIdFields(rcon2), 
                 c("record_id", "text_test"))
  }
)

test_that("Does not return secondary field that is not in current fields",
  {
    # Force a secondary unique field
    rcon2 <- rcon
    NewInfo <- data.frame(secondary_unique_field = "does_not_exist")
    importProjectInformation(rcon2, NewInfo)
    rcon2$refresh_projectInformation()
    
    expect_warning(ids <- getProjectIdFields(rcon2), "secondary unique field that does not exist")
    expect_equal(ids, "record_id")
  }
)

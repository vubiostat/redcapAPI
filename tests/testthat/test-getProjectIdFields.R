context("getProjectIdFields")

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
    
    # Now let's make a secondary unique field
    NewInfo <- data.frame(secondary_unique_field = "text_test")
    importProjectInformation(rcon, NewInfo)
    rcon$refresh_projectInformation()
    
    expect_equal(getProjectIdFields(rcon), 
                 c("record_id", "text_test"))
    
    NewInfo <- data.frame(secondary_unique_field = "", 
                          surveys_enabled = 0)
    importProjectInformation(rcon, NewInfo)
    rcon$refresh_projectInformation()
  }
)

context("exportArms")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Return an error if rcon is not a redcapConnectionObject", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportArms("not an rcon"), 
                 "no applicable method for 'exportArms'")
  }
)

test_that(
  "Return an error if arms is not a character vector", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportArms(rcon, pi), 
                 "Variable 'arms'[:] Must be of type 'character'")
  }
)


test_that("Returns a data frame with two rows for a study with two arms",{
  is_longitudinal <- 
    as.logical(exportProjectInformation(rcon)$is_longitudinal)
  
  expect_data_frame(exportArms(rcon), 
                    nrows = if (is_longitudinal) 2 else 0)
})


test_that(
  "Returns a data frame for specific arms",
  {
    is_longitudinal <- 
      as.logical(exportProjectInformation(rcon)$is_longitudinal)
    
    skip_if(!is_longitudinal, 
            "Test project is not a longitudinal project. This test is skipped")
    
    expect_data_frame(exportArms(rcon, arms = c("1")), 
                      ncols = 2, 
                      nrows = 1)
  }
)


test_that(
  "Returns empty data frame for a classic project",
  {
    # We will alter the project information and push it into the 
    # cached value in order to mimic the state of a non-longitudinal project
    tmp_proj <- rcon$projectInformation()
    tmp_proj$is_longitudinal <- 0
    
    rcon$push_projectInformation(tmp_proj)
    
    expect_data_frame(exportArms(rcon),
                      ncols = 2,
                      nrows = 0)
    rcon$flush_projectInformation()
  }
)



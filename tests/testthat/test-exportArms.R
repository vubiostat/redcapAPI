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
  
  skip_if(!is_longitudinal, 
          "Test project is not a longitudinal project. This test is skipped")
  
  expect_data_frame(exportArms(rcon), 
                    nrows = 2)
})


test_that(
  "Returns a data frame for specific arms",
  {
    is_longitudinal <- 
      as.logical(exportProjectInformation(rcon)$is_longitudinal)
    
    skip_if(TRUE,
            "Throwing an error when arms is specified. Revisit this test after correcting.")
    
    skip_if(!is_longitudinal, 
            "Test project is not a longitudinal project. This test is skipped")
    
    exportArms(rcon, arms = c("1"))
  }
)


test_that(
  "Returns NULL for a classic project",
  {
    is_longitudinal <- 
      as.logical(exportProjectInformation(rcon)$is_longitudinal)
    
    skip_if(is_longitudinal, 
            "Test project is a longitudinal project. This test is skipped")
    
    expect_null(exportArms(rcon))
  }
)



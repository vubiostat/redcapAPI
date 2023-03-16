context("exportMappings.R")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Return an error when rcon is not a redcapConnection object", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportMappings("not an rcon"), 
                 "no applicable method for 'exportMappings'")
  }
)

test_that(
  "Return an error when arms is not character",
  {
    local_reproducible_output(width = 200)
    expect_error(exportMappings(rcon, 
                                arms = 1:2), 
                 "Variable 'arms'[:] Must be of type 'character'")
  }
)


test_that(
  "Return a data frame of mappings when called with defaults (longitudinal)", 
  {
    is_longitudinal <- 
      as.logical(exportProjectInformation(rcon)$is_longitudinal)
    
    skip_if(!is_longitudinal, 
            "Test project is not a longitudinal project. This test is skipped")
    
    expect_data_frame(exportMappings(rcon), 
                      ncols = 3)
  }
)

test_that(
  "Return NULL for a classial project", 
  {
    is_longitudinal <- 
      as.logical(exportProjectInformation(rcon)$is_longitudinal)
    
    skip_if(is_longitudinal, 
            "Test project is a longitudinal project. This test is skipped")
    
    expect_null(exportMappings(rcon))
  }
)

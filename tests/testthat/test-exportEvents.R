context("exportEvents.R")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Return an error when rcon is not a redcapConnection object", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportEvents("not an rcon"), 
                 "no applicable method for 'exportEvents'")
  }
)

test_that(
  "Return an error when arms is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportEvents(rcon, 
                              arms = pi), 
                 "Variable 'arms'[:] Must be of type 'character'")
  }
)

test_that(
  "Returns a data frame of events (Longitudinal project)", 
  {
    is_longitudinal <- 
      as.logical(exportProjectInformation(rcon)$is_longitudinal)
    
    skip_if(!is_longitudinal, 
            "Test project is not a longitudinal project. This test is skipped")
    
    expect_data_frame(exportEvents(rcon), 
                      ncols = 5)
  }
)

test_that(
  "Returns a data frame of events for a specific arms", 
  {
    is_longitudinal <- 
      as.logical(exportProjectInformation(rcon)$is_longitudinal)
    
    skip_if(!is_longitudinal, 
            "Test project is not a longitudinal project. This test is skipped")
    
    expect_data_frame(exportEvents(rcon, 
                                   arms = c("2")), 
                      ncols = 5, 
                      nrow = 1)
      
  }
)


test_that(
  "Returns NULL for classical projects", 
  {
    is_longitudinal <- 
      as.logical(exportProjectInformation(rcon)$is_longitudinal)
    
    skip_if(is_longitudinal, 
            "Test project is a longitudinal project. This test is skipped")
    
    expect_null(exportEvents(rcon))
  }
)

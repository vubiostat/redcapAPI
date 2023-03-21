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
    
    expect_data_frame(exportEvents(rcon, 
                                   arms = c("1", "2")), 
                      ncols = 5, 
                      nrow = 3)
      
  }
)


test_that(
  "Returns NULL for classical projects", 
  {
    # We will alter the project information and push it into the 
    # cached value in order to mimic the state of a non-longitudinal project
    # This only works because the API isn't every called for a non-longitudinal project
    tmp_proj <- rcon$projectInformation()
    tmp_proj$is_longitudinal <- 0
    
    rcon$push_projectInformation(tmp_proj)
    
    expect_data_frame(exportEvents(rcon), 
                      ncols = 5, 
                      nrows = 0)
    
    rcon$flush_projectInformation()
  }
)

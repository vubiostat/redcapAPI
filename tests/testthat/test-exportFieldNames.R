context("exportFieldsNames.R")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Return an error when rcon is not a redcapConnection object", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFieldNames("not an rcon"), 
                 "no applicable method for 'exportFieldNames'")
  }
)


test_that(
  "Return an error when fields is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(
      exportFieldNames(rcon, 
                       fields = 1:2), 
      "'fields'[:] Must be of type 'character'"
    )
  }
)

test_that(
  "Return an error if bundle is not of class redcapBundle",
  {
    local_reproducible_output(width = 200)
    expect_error(
      exportFieldNames(rcon, 
                       bundle = "this is not a bundle"), 
      "Must inherit from class 'redcapBundle'"
    )
  }
)


test_that(
  "Return a data frame when called with defaults",
  {
    expect_data_frame(
      exportFieldNames(rcon),
      ncols = 3
    )
  }
)

test_that(
  "Return a data frame with only requested field names", 
  {
    expect_data_frame(
      exportFieldNames(rcon, 
                       fields = c("record_id")), 
      ncols = 3, 
      nrows = 1
    )
    
    expect_data_frame(
      exportFieldNames(rcon, 
                       fields = c("prereq_radio")), 
      ncols = 3, 
      nrows = 1
    )
    
    expect_data_frame(
      exportFieldNames(rcon, 
                       fields = c("prereq_checkbox")), 
      ncols = 3, 
      nrows = 4
    )
  }
)

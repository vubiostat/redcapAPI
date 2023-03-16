context("exportProjectInformation.R")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    expect_error(exportProjectInformation("not an rcon"), 
                 "no applicable method for 'exportProjectInformation'")
  }
)

test_that(
  "Return a data frame with 1 row", 
  {
    # If this suddenly starts failing, look to see if the API has 
    # added more columns. Hard coding the number of columns in the 
    # test may be a bit of a fool's move (sorry).
    expect_data_frame(exportProjectInformation(rcon), 
                      ncols = 26,
                      nrows = 1)
  }
)

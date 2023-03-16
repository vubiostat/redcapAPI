context("exportInstruments.R")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Return an error when rcon is not a redcapConnection object", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportInstruments("not an rcon"), 
                 "no applicable method for 'exportInstruments'")
  }
)

test_that(
  "Returns a data frame of instruments", 
  {
    expect_data_frame(exportInstruments(rcon), 
                      ncols = 2)
  }
)

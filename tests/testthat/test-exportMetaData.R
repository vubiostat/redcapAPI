context("exportMetaData.R")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Return an error when rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportMetaData("not an rcon"), 
                 "no applicable method for 'exportMetaData'")
  }
)

test_that(
  "Return an error when fields is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportMetaData(rcon, fields = 1:3), 
                 "'fields'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error when forms is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportMetaData(rcon, forms = 1:3), 
                 "'forms'[:] Must be of type 'character'")
  }
)


test_that(
  "Return a data frame when called with defaults", 
  {
    expect_data_frame(exportMetaData(rcon), 
                      ncols = 18)
  }
)

test_that(
  "Return a data frame of only selected fields", 
  {
    expect_data_frame(
      exportMetaData(rcon, 
                     fields = c("record_id", "date_dmy", "prereq_radio")), 
      ncols = 18, 
      nrows = 3
    )
  }
)

test_that(
  "Return a data frame of only selected forms", 
  {
    expect_data_frame(
      exportMetaData(rcon, 
                     forms = c("fieldtovar_datetimes", "randomization")), 
      ncols = 18, 
      nrows = 14
    )
  }
)


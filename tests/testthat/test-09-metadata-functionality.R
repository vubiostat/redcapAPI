context("Export / Import MetaData Functionality")

# FIXME: realign tests when importMetaData is written

rcon <- redcapConnection(url = url, token = API_KEY)

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

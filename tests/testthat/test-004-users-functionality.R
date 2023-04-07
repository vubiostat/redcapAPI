context("Export/Import/Delete Users Functionality")

# FIXME: Expand testing after writing import and delete methods

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Returns a data frame when called with defaults", 
  {
    User <- exportUsers(rcon)
    expect_data_frame(User)
    expect_true(ncol(User) > 36)
  }
)

test_that(
  "Returns a data frame with dates = FALSE", 
  {
    expect_data_frame(exportUsers(rcon, 
                                  dates = FALSE))
  }
)

test_that(
  "Returns a data frame with labels = FALSE", 
  {
    expect_data_frame(exportUsers(rcon,  
                                  labels = FALSE))
  }
)

test_that(
  "Returns a data frame with form_rights = FALSE", 
  {
    expect_data_frame(exportUsers(rcon,  
                                  form_rights = FALSE))
  }
)


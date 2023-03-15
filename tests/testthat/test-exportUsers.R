context("exportUsers.R")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    expect_error(exportUsers("not an rcon"), 
                 "no applicable method for 'exportUsers'")
  }
)

test_that(
  "Return an error if dates is not logical(1)", 
  {
    expect_error(exportUsers(rcon, 
                             dates = c(TRUE, FALSE)), 
                 "'dates'[:] Must have length 1")
    expect_error(exportUsers(rcon, 
                             dates = "TRUE"), 
                 "'dates'[:] Must be of type 'logical'")
  }
)


test_that(
  "Return an error if labels is not logical(1)", 
  {
    expect_error(exportUsers(rcon, 
                             labels = c(TRUE, FALSE)), 
                 "'labels'[:] Must have length 1")
    expect_error(exportUsers(rcon, 
                             labels = "TRUE"), 
                 "'labels'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if bundle is not a redcapBundle", 
  {
    expect_error(exportUsers(rcon, 
                             bundle = "not a bundle"), 
                 "'bundle'[:] Must inherit from class 'redcapBundle'")
  }
)


test_that(
  "Returns a data frame when called with defaults", 
  {
    skip_if(TRUE, 
            "There is an error when labels = TRUE. Revisit this test when corrected.")
    expect_data_frame(exportUsers(rcon))
  }
)

test_that(
  "Returns a data frame with dates = FALSE", 
  {
    expect_data_frame(exportUsers(rcon, 
                                  dates = FALSE, 
                                  labels = FALSE))
  }
)

test_that(
  "Returns a data frame with labels = FALSE", 
  {
    expect_data_frame(exportUsers(rcon,  
                                  labels = FALSE))
  }
)


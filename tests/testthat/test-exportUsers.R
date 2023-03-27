context("exportUsers.R")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportUsers("not an rcon"), 
                 "no applicable method for 'exportUsers'")
  }
)

test_that(
  "Return an error if dates is not logical(1)", 
  {
    local_reproducible_output(width = 200)
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
    local_reproducible_output(width = 200)
    expect_error(exportUsers(rcon, 
                             labels = c(TRUE, FALSE)), 
                 "'labels'[:] Must have length 1")
    expect_error(exportUsers(rcon, 
                             labels = "TRUE"), 
                 "'labels'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if form_rights is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportUsers(rcon, 
                             form_rights = c(TRUE, FALSE)), 
                 "'form_rights'[:] Must have length 1")
    expect_error(exportUsers(rcon, 
                             form_rights = "TRUE"), 
                 "'form_rights'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if bundle is not a redcapBundle", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportUsers(rcon, 
                             bundle = "not a bundle"), 
                 "'bundle'[:] Must inherit from class 'redcapBundle'")
  }
)

test_that(
  "Return an error if config or api_param are not named lists", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportUsers(rcon, 
                             config = list("parameter")), 
                 "'config'[:] Must have names")
    expect_error(exportUsers(rcon, 
                             api_param = list("parameter")), 
                 "'api_param'[:] Must have names")
  }
)


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

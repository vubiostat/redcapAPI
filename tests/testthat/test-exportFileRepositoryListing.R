context("exportFileRepositoryListing")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Returns a data frame", 
  {
    expect_data_frame(exportFileRepositoryListing(rcon), 
                      ncols = 4)
    expect_data_frame(exportFileRepositoryListing(rcon, 
                                                  recursive = TRUE), 
                      ncols = 4)
  }
)

test_that(
  "Return an error if rcon is not a redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFileRepositoryListing(mtcars), 
                 "no applicable method for 'exportFileRepositoryListing'")
  }
)

test_that(
  "Return an error if folder_id is not numeric(0/1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFileRepositoryListing(rcon, 
                                             folder_id = c(104, 105)), 
                 "'folder_id'[:] Must have length [<][=] 1")
    expect_error(exportFileRepositoryListing(rcon, 
                                             folder_id = "104"), 
                 "'folder_id'[:] Must be of type 'integerish'")
    expect_error(exportFileRepositoryListing(rcon, 
                                             folder_id = pi), 
                 "'folder_id'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error if recursive is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFileRepositoryListing(rcon, 
                                             recursive = c(TRUE, FALSE)), 
                 "'recursive'[:] Must have length 1")
    expect_error(exportFileRepositoryListing(rcon, 
                                             recursive = "TRUE"), 
                 "'recursive'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if config or api_param are not named lists", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportFileRepositoryListing(rcon, 
                                             config = list("parameter")), 
                 "'config'[:] Must have names")
    expect_error(exportFileRepositoryListing(rcon, 
                                             api_param = list("parameter")), 
                 "'api_param'[:] Must have names")
  }
)
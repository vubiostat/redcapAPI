context("exportVersion.R")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    expect_error(exportVersion("not an rcon"), 
                 "no applicable method for 'exportVersion'")
  }
)

test_that(
  "exportVersion returns a version number", 
  {
    expect_silent(version <- exportVersion(rcon))
    expect_true(grepl("\\d{1,4}[.]\\d{1,4}[.]\\d{1,4}", version))
  }
)

context("exportReports")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that("reports can be exported",{
  expect_silent(rep <- exportReports(rcon, 357209))
})

test_that(
  "fields in the drop= arg are not returned", 
  {
    fields_to_drop <- c("treatment")
    Report <- exportReports(rcon, 357209,
                             drop = fields_to_drop)
    expect_false("treatment" %in% names(Report))
  }
)


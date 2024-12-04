context("redcapConnection Functionality")

test_that("redcapApiConnection can be created",
  expect_class(
    redcapConnection(url = url, token = 'YO'),
    classes = c("redcapApiConnection", "redcapConnection")
  )
)



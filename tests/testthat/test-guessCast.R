context("guessCast")

conn <- offlineConnection()

x <- data.frame(
  x=c("xyz", "2023-01-01", "", "2003-12-12", "2003-12-12")
)


test_that(
  "no guess cast below threshold",
  {
    expect_equal(x[1:4,], guessDate(x[1:4,], conn))
  }
)


test_that(
  "guess cast above threshold",
  {
    y <- guessDate(x, conn)
    expect_class(y$x, "POSIXct")
  }
)

test_that(
  "guess cast gives message when triggered",
  {
    expect_message(guessDate(x, conn), "guessCast")
  }
)

test_that(
  "guess cast respects_quiet",
  {
    expect_message(guessDate(x, conn, quiet=TRUE), NA)
  }
)


test_that(
  "guess cast reports invalid",
  {
    y <- guessDate(x, conn)
    expect_class(attr(y, "invalid"), "invalid")
  }
)
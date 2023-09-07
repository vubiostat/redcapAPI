context("guessCast.R")

x <- data.frame(
  x=c("xyz", "2023-01-01", "", "2003-12-12", "2003-12-12", "2012-10-10")
)


test_that(
  "no guess cast below threshold",
  {
    y <- guessDate(x[1:4,,drop=FALSE], rcon)
    expect_class(y$x, "character")
  }
)


test_that(
  "guess cast above threshold",
  {
    y <- guessDate(x, rcon)
    expect_class(y$x, "POSIXct")
  }
)

test_that(
  "guess cast gives message when triggered",
  {
    expect_message(guessDate(x, rcon), "guessCast")
  }
)

test_that(
  "guess cast respects_quiet",
  {
    expect_message(guessDate(x, rcon, quiet=TRUE), NA)
  }
)


test_that(
  "guess cast reports invalid",
  {
    y <- guessDate(x, rcon)
    expect_class(attr(y, "invalid"), "invalid")
  }
)

test_that(
  "guess cast validates arguments",
  {
    expect_error(guessDate(1:3, rcon, quiet=TRUE), "Variable 'data'")
    expect_error(guessDate(x, 1:3, quiet=TRUE), "Variable 'rcon'")
    expect_error(guessDate(x, rcon, quiet=1.3), "Variable 'quiet'")
    expect_error(guessDate(x, rcon, quiet=TRUE,na=TRUE), "Variable 'na'")
    expect_error(guessDate(x, rcon, quiet=TRUE,validation=TRUE), "Variable 'validation'")
    expect_error(guessDate(x, rcon, quiet=TRUE,cast=TRUE), "Variable 'cast'")
    expect_error(guessDate(x, rcon, quiet=TRUE,threshold=TRUE), "Variable 'threshold'")
    
  }
)

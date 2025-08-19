context("unlockREDCap")

library(mockery)
library(curl)

url     <- rcon$url   # Should not be required but it is


h <- new_handle(timeout = 1L)
redirect <- structure(
  list(url = "https://test.xyz/api/",
       status_code = 302L,
       content = writeBin("", raw()),
       headers=structure(list(
         'content-type'="text/csv; charset=utf-8",
         'location'=url
       ),
       class = c("insensitive", "list")),
  class = "response")
)

test_that(
  "connectAndCheck returns result of redcapConnection",
  {
    stub(connectAndCheck, "redcapConnection", rcon)

    expect_identical(connectAndCheck("key", "url"), rcon)
  }
)

test_that(
  "connectAndCheck deals with redirect 301 status",
  {
    redirectCall <- TRUE
    redirect$status_code = 301L
    stub(connectAndCheck, "makeApiCall", function(...)
      if(redirectCall) { redirectCall <<- FALSE; redirect  } else {makeApiCall(...)})

    rcon <- connectAndCheck(rcon$token, "https://test.xyz/api/")
    expect_equal(rcon$url, url)
  }
)

test_that(
  "connectAndCheck deals with redirect 302 status",
  {
    redirectCall <- TRUE
    stub(connectAndCheck, "makeApiCall", function(...)
      if(redirectCall) { redirectCall <<- FALSE; redirect  } else {makeApiCall(...)})

    rcon <- connectAndCheck(rcon$token, "https://test.xyz/api/")
    expect_equal(rcon$url, url)
  }
)

test_that(
  "connectAndCheck does not allow for more than one redirect",
  {
    stub(connectAndCheck, "makeApiCall", redirect)

    expect_error(connectAndCheck(rcon$token, "https://test.xyz/api"))
  }
)

test_that(
  "connectAndCheck returns NULL on a 403 (Invalid KEY)",
  {
    stub(connectAndCheck, "redcapConnection",
                  mock(stop("403")))
    expect_true(is.null(connectAndCheck("key", "url")))
  }
)

test_that(
  "connectAndCheck errors with bad url",
  expect_error(connectAndCheck("key", "badurl"), "Invalid URL provided")
)

test_that(
  "connectAndCheck errors with valid url but not a REDCap server",
  expect_error(connectAndCheck("key", "https://google.com"), "refused connection")
)

test_that(
  "connectAndCheck errors if given HTML and not a version",
  {
    local_reproducible_output(width = 200)
    stub(connectAndCheck, "makeApiCall", function(...)
    {
      structure(
        list(url = "https://test.xyz/api/",
             status_code = 200L,
             content = writeBin("<html></html>", raw()),
             headers=structure(list(
               'content-type'="text/csv; charset=utf-8",
               'location'=url
             ),
             class = c("insensitive", "list")),
        class = "response")
      )
    })
    stub(connectAndCheck, "browseURL", function(...) {})
    expect_error(connectAndCheck("key", "https://test.xyz/api/"), "Server URL responded with web page")
  }
)

test_that(
  "unlockREDCap pulls API_KEY and opens connection from keyring returning as list",
  {
    skip_if(Sys.getenv("CI") == "1",
            "CI cannot test user interactions")

    expect_silent(x <- unlockREDCap(c(rcon=testdb), url, keyring="API_KEYs"))
    expect_true("rcon" %in% names(x))
    expect_class(x$rcon, "redcapApiConnection")
  }
)

test_that(
  "unlockREDCap pulls API_KEY and opens connection from keyring written to env",
  {
    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1; ""}
    e <- new.env()

    expect_silent(unlockREDCap(
      c(rcon=testdb), url, keyring="API_KEYs",
      envir=e, passwordFUN=passwordFUN))

    expect_true(exists("rcon", envir=e))
    expect_class(e[["rcon"]], "redcapApiConnection")
    expect_true(calls == 0) # No password requests
  }
)

context("redcapError Functionality")

test_that(
  "Return null for certain errors", 
  {
    return_as_null <- c("ERROR: The value of the parameter \"content\" is not valid",
                        "ERROR: You cannot export arms for classic projects",
                        "ERROR: You cannot export events for classic projects",
                        "ERROR: You cannot export form/event mappings for classic projects")
    options(redcap_error_handling=NULL)
    lapply(return_as_null, 
           function(x) expect_null(redcapError(x)))
    
    # and now halt execution for special old conditions
    options(redcap_error_handling='error')

    lapply(return_as_null, 
           function(x) expect_error(redcapError(x)))
    options(redcap_error_handling=NULL)
  }
)

test_that(
  "Return preferred error message when experiencing a timeout", 
  {
    expect_error(redcapError("Connection reset by peer"), 
                 "A network error has occurred")
    expect_error(redcapError("Timeout was reached"), 
                 "A network error has occurred")
  }
)

test_that(
  "Return the error if it isn't one of the handled errors", 
  {
    response <- makeApiCall(rcon, body = list(content = 'no-content', 
                                              returnFormat = 'csv'))
    response$content <- charToRaw("my error")
    response$status_code <- 701

    expect_error(redcapError(response), 
                 "701: my error")
  }
)

context("Test package setup")

test_that(
  "Default timeout set to 5 minutes (300 seconds / 30,000 milliseconds)", 
  {
    old <- httr::set_config(httr::timeout(500))
    
    expect_equal(old$options$timeout_ms, 
                 3e+05)
    
    httr::set_config(old)
  }
)


test_that(
  "Setting timeout on the connection object overrides the default", 
  {
    this_conn <- redcapConnection(url = rcon$url, 
                                  token = rcon$token, 
                                  config = httr::config(timeout_ms = 5e+05))
    
    default_response <- makeApiCall(rcon, 
                                    body = list(content = "metadata", 
                                                return = "csv", 
                                                returnFormat = "csv"))
    
    custom_response <- makeApiCall(this_conn, 
                                   body = list(content = "metadata", 
                                               return = "csv", 
                                               returnFormat = "csv"))
    
    expect_equal(default_response$request$options$timeout_ms, 
                 3e+05)
    expect_equal(custom_response$request$options$timeout_ms, 
                 5e+05)
    
  }
)

test_that(
  "Setting timeout on the function call overrides the default", 
  {
    this_conn <- redcapConnection(url = rcon$url, 
                                  token = rcon$token, 
                                  config = httr::config(timeout_ms = 5e+05))
    
    default_response <- makeApiCall(rcon, 
                                    body = list(content = "metadata", 
                                                return = "csv", 
                                                returnFormat = "csv"))
    
    custom_response <- makeApiCall(this_conn, 
                                   body = list(content = "metadata", 
                                               return = "csv", 
                                               returnFormat = "csv"), 
                                   config = httr::config(timeout_ms = 4e+05))
    
    expect_equal(default_response$request$options$timeout_ms, 
                 3e+05)
    expect_equal(custom_response$request$options$timeout_ms, 
                 4e+05)
    
  }
)

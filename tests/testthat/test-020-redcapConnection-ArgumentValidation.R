context("redcapConnection Argument Validation")

API_KEY <- rcon$token
url     <- rcon$url   # Should not be required but it is

#####################################################################
# Argument Validation                                            ####

test_that(
  "redcapConnection throws an  error if config is not a named list",
  {
    local_reproducible_output(width = 200)

    expect_error(redcapConnection(url = url, 
                                  token = API_KEY, 
                                  config = list(1)), 
                 "'config': Must have names")
    expect_error(redcapConnection(url = url, 
                                  token = API_KEY, 
                                  config = "not a list"), 
                 "'config': Must be of type 'list'")
  }
)

test_that(
  "redcapConnection throws an error if url is missing",
  {
    local_reproducible_output(width = 200)
    expect_error(redcapConnection(token = API_KEY))
  }
)

test_that(
  "redcapConnection throws an  error if token is missing",
  {
    local_reproducible_output(width = 200)
    expect_error(redcapConnection(url = url))
  }
)

test_that(
  "return an error if retries is not integerish(1) > 0", 
  {
    local_reproducible_output(width = 200)
    expect_error(redcapConnection(url = url, 
                                  token = API_KEY, 
                                  retries = "5"), 
                 "Variable 'retries': Must be of type 'integerish'")
    
    expect_error(suppressWarnings(redcapConnection(url = url, 
                                                   token = API_KEY, 
                                                   retries = 1:5)), 
                 "'retries': Must have length 1")
    
    expect_error(redcapConnection(url = url, 
                                  token = API_KEY, 
                                  retries = 0), 
                 "Variable 'retries': Element 1 is not [>][=] 1")
  }
)

test_that(
  "return an error if retry_interval is not numeric >= 0", 
  {
    local_reproducible_output(width = 200)
    expect_error(redcapConnection(url = url, 
                                  token = API_KEY, 
                                  retry_interval = "0"), 
                 "'retry_interval': Must be of type 'numeric'")
    
    expect_error(redcapConnection(url = url, 
                                  token = API_KEY, 
                                  retry_interval = -1), 
                 "'retry_interval': Element 1 is not [>][=] 0")
  }
)

test_that(
  "return an error if retry_quietly is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(redcapConnection(url = url, 
                                  token = API_KEY, 
                                  retry_quietly = c(TRUE, FALSE)), 
                 "'retry_quietly': Must have length 1")
    
    expect_error(redcapConnection(url = url, 
                                  token = API_KEY, 
                                  retry_quietly = "TRUE"), 
                 "'retry_quietly': Must be of type 'logical'")
  }
)

context("makeApiCall Argument Validation")

library(mockery)
library(curl)

# Note: This file will only test that arguments fail appropriately, or
# that submethods perform as expected. the makeApiCall function 
# is ubiquitous throughout the package. If we break it, it's bound
# to pop up in other tests.

# Test .makeApiCall_validateResponse

h <- new_handle(timeout = 1L)
goodVersionPOST <- structure(list(url = "https://redcap.vanderbilt.edu/api/", status_code = 200L, 
    headers = structure(list(date = "Mon, 09 Oct 2023 20:24:57 GMT", 
        expires = "0", `cache-control` = "no-store, no-cache, must-revalidate", 
        pragma = "no-cache", `x-xss-protection` = "1; mode=block", 
        `x-content-type-options` = "nosniff", `access-control-allow-origin` = "*", 
        `strict-transport-security` = "max-age=31536000", `redcap-random-text` = "weFkyMRUge5eZCiWyRT7dXCybPGz9DAKTUsW5kqa2u2", 
        `content-encoding` = "gzip", vary = "Accept-Encoding", 
        `content-type` = "text/csv; charset=utf-8", `transfer-encoding` = "chunked"), class = c("insensitive", 
    "list")), all_headers = list(list(status = 200L, version = "HTTP/1.1", 
        headers = structure(list(date = "Mon, 09 Oct 2023 20:24:57 GMT", 
            expires = "0", `cache-control` = "no-store, no-cache, must-revalidate", 
            pragma = "no-cache", `x-xss-protection` = "1; mode=block", 
            `x-content-type-options` = "nosniff", `access-control-allow-origin` = "*", 
            `strict-transport-security` = "max-age=31536000", 
            `redcap-random-text` = "hardyharhar", 
            `content-encoding` = "gzip", vary = "Accept-Encoding", 
            `content-type` = "text/csv; charset=utf-8", `transfer-encoding` = "chunked"), class = c("insensitive", 
        "list")))), cookies = structure(list(domain = c("#HttpOnly_redcap.vanderbilt.edu", 
    "redcap.vanderbilt.edu"), flag = c(FALSE, FALSE), path = c("/", 
    "/"), secure = c(TRUE, FALSE), expiration = structure(c(Inf, 
    Inf), class = c("POSIXct", "POSIXt")), name = c("BIGipServer~legacy_services~redcap_443", 
    "babble"), value = c("babble", "secrets"
    )), row.names = c(NA, -2L), class = "data.frame"), content = as.raw(c(0x31, 
    0x33, 0x2e, 0x31, 0x30, 0x2e, 0x33)), date = structure(1696883097, class = c("POSIXct", 
    "POSIXt"), tzone = "GMT"), times = c(redirect = 0, namelookup = 0.001079, 
    connect = 0.022278, pretransfer = 0.119255, starttransfer = 0.119258, 
    total = 0.445047), request = structure(list(method = "POST", 
        url = "https://redcap.vanderbilt.edu/api/", headers = c(Accept = "application/json, text/xml, application/xml, */*"), 
        fields = list(token = "DIDNTSAYTHEMAGICWORD", 
            content = "version", format = "csv"), options = list(
            useragent = "libcurl/7.81.0 r-curl/5.0.2 httr/1.4.7", 
            timeout_ms = 3e+05, post = TRUE), auth_token = NULL, 
        output = structure(list(), class = c("write_memory", 
        "write_function"))), class = "request"), handle = h), class = "response")

test_that(
  ".makeApiCall_isRetryEligible returns appropriate logical values", 
  {
    response <- makeApiCall(rcon, 
                            body = list(content = "metadata", 
                                        format = "csv"))
    
    # list of codes from   https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
    all_codes <- c(100, 101, 102, 103, 
                   200, 201, 202, 203, 304, 205, 206, 207, 208, 226, 
                   300, 301, 302, 303, 304, 305, 306, 307, 308, 
                   400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 
                   416, 417, 418, 421, 422, 423, 424, 425, 426, 428, 429, 431, 451, 
                   500, 501, 502, 503, 504, 505, 506, 507, 508, 510, 511)
    
    eligible_codes <- c(408, 500, 502, 503, 504)
    
    ideal <- all_codes %in% eligible_codes
    
    actual <- rep(FALSE, length(all_codes))
    
    for (i in seq_along(all_codes)){
      response$status_code <- all_codes[i]
      actual[i] <- .makeApiCall_isRetryEligible(response)
    }
    
    expect_equal(ideal, actual)
  }
)

test_that(
  "makeApiCall deals with curl timeouts",
  {
    e <- structure(list(message = "Timeout was reached: [redcap.vanderbilt.edu] Operation timed out after 300001 milliseconds with 0 bytes received", 
                        call = curl_fetch_memory("https://redcap.vanderbilt.edu/api/params", 
                                                 handle = h)), class = c("simpleError", "error", "condition"
                                                 ))
    
    x <- 1
    stub(makeApiCall, "httr::POST", function(...)
      if(x==1) { x <<- 2; stop(e) } else {goodVersionPOST})
    
    response <- makeApiCall(rcon, 
                            body = list(content = "version", 
                                        format = "csv"))
    
    expect_true(response$status==200)
  }
)

test_that(
  ".makeApiCall_retryMessage gives appropriate messages", 
  {
    response <- makeApiCall(rcon, 
                            body = list(content = "invalid-content", 
                                        format = "csv"))
    
    expect_message(.makeApiCall_retryMessage(rcon, response, 1), 
                   "API attempt 1 of 5 failed. Trying again in 2 seconds. ERROR: The value of the parameter \"content\" is not valid")
    
    expect_message(.makeApiCall_retryMessage(rcon, response, rcon$retries()), 
                   sprintf("API attempt %s of %s failed. ERROR: The value of the parameter \"content\" is not valid", 
                           rcon$retries(), rcon$retries()))
  }
)

test_that(
  "Return custom error message when reset by peer (Issue 181)", 
  {
    response <- makeApiCall(rcon, 
                            body = list(content = "arm", 
                                        format = "csv", 
                                        returnFormat = "csv"))
    response$status_code <- 502
    response$content <- charToRaw("Recv failure: Connection reset by peer")
    
    expect_error(redcapError(response, "null"), 
                 "A network error has occurred. This can happen when too much data is")
    
    response$content <- charToRaw("Timeout was reached: [redcap.vanderbilt.edu] SSL connection timeout")
    
    expect_error(redcapError(response, "null"), 
                 "A network error has occurred. This can happen when too much data is")
    
  }
)

#####################################################################
# Argument Validation

test_that(
  "Return an error if rcon is not of class redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(makeApiCall(mtcars), 
                 "'rcon': Must inherit from class 'redcapApiConnection'")
  }
)

test_that(
  "Return an error if body is not a named list", 
  {
    local_reproducible_output(width = 200)
    expect_error(makeApiCall(rcon, 
                             letters), 
                 "'body': Must be of type 'list'")
    
    expect_error(makeApiCall(rcon, 
                             list(1, 2, 3)), 
                 "'body': Must have names")
  }
)

test_that(
  "Return an error if config is not a named list", 
  {
    local_reproducible_output(width = 200)
    expect_error(makeApiCall(rcon, 
                             config = letters), 
                 "'config': Must be of type 'list'")
    
    expect_error(makeApiCall(rcon, 
                             config = list(1, 2, 3)), 
                 "'config': Must have names")
  }
)
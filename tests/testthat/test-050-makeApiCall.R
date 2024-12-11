context("makeApiCall Argument Validation")

library(mockery)
library(curl)

url     <- rcon$url   # Should not be required but it is


# Note: This file will only test that arguments fail appropriately, or
# that submethods perform as expected. the makeApiCall function 
# is ubiquitous throughout the package. If we break it, it's bound
# to pop up in other tests.

# Test .makeApiCall_validateResponse

test_that(
  "makeApiCall will not allow a non-character url",
  {
    expect_error(
      makeApiCall(rcon,  body = list(format = "csv"), url=TRUE),
      "url.*Must be of type 'character'")
  }
)

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
  "makeApiCall handles curl timeout with translated error",
  {
    h <- new_handle(timeout = 1L)
    e <- structure(
      list(message = paste0("Timeout was reached: [", url,
                            "] Operation timed out after 300001 milliseconds with 0 bytes received"), 
           call = 'curl_fetch_memory(paste0(url,"/params"), handle = h)'
      ),
      class = c("simpleError", "error", "condition")
    )
    
    rcon$set_retries(1)
    x <- 1
    stub(makeApiCall, ".curlPost", function(...)
      if(x==1) { x <<- 2; stop(e) } else {x <<- 3; goodVersionPOST})
    
    expect_error(
      makeApiCall(rcon, body=list(content = "version", format = "csv"), 
      "A network error has occurred"))
    expect_equal(x, 2)
    rcon$set_retries(5)
  }
)

test_that(
  "makeApiCall recovers from curl timeout gracefully",
  {
    h <- new_handle(timeout = 1L)
    goodVersionPOST <- structure(
      list(url = url,
           status_code = 200L,
           content = charToRaw("13.10.3"),
           headers=structure(list(
             'Content-Type'="text/csv; charset=utf-8"
           )),
           class = c("insensitive", "list")),
      class = "response")
    e <- structure(
           list(message = paste0("Timeout was reached: [", url,
                            "] Operation timed out after 300001 milliseconds with 0 bytes received"),
                call = 'curl_fetch_memory(paste0(url,"/params"), handle = h)'
                ),
           class = c("simpleError", "error", "condition")
    )
    
    x <- 1
    stub(makeApiCall, ".curlPost", function(...)
      if(x==1) { x <<- 2; stop(e) } else {x <<- 3; goodVersionPOST})
    
    response <- makeApiCall(rcon, 
                            body = list(content = "version", 
                                        format = "csv"))
    
    expect_equal(x, 3)
    expect_true(response$status==200)
  }
)

test_that(
  ".makeApiCall_retryMessage gives appropriate messages", 
  {
    
    expect_message(.makeApiCall_retryMessage(rcon, "msg", 1), 
                   "API attempt 1 of 5 failed. Trying again in 2 seconds. msg")
    
    expect_message(.makeApiCall_retryMessage(rcon, "msg", rcon$retries()), 
                   sprintf("API attempt %s of %s failed. msg", 
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
    
    expect_error(redcapError(response), 
                 "A network error has occurred. This can happen when too much data is")
    
    response$content <- charToRaw(paste0("Timeout was reached: [",url,"] SSL connection timeout"))
    
    expect_error(redcapError(response), 
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

test_that(
  "success_status_codes must be integerish",
  {
    expect_error(makeApiCall(rcon, success_status_code = TRUE),
                 "'success_status_codes': Must be of type 'integerish'")
  }
)

test_that(
  "as.data.frame.response handles invalid encoded characters",
  {
    x <- list(content=charToRaw("fa\xE7il,joe\n1,2\xE7\n3,4"))
    x[['headers']] <- list('content-type'='text/csv; charset=utf-8')
    class(x) <- c("response","list")
    expect_warning({y <- redcapAPI:::as.data.frame.response(x)},
                  "invalid characters")
    expect_equal(
      y,
      data.frame(fa.il=as.integer(c(1,3)), joe=c("2\U25a1","4"))
    )
    
    x[['headers']] <- list('content-type'='text/csv') # defaults to latin
    expect_silent({y <- redcapAPI:::as.data.frame.response(x)})
    expect_equal(
      y,
      data.frame(façil=as.integer(c(1,3)), joe=c("2ç","4"))
    )
    
  }
)

test_that(
  "makeApiCall handles permanent redirect",
  {
    local_reproducible_output(width = 200)
    h <- new_handle(timeout = 1L)
    redirect <- structure(
      list(url = "https://test.xyz/api",
           status_code = 301L,
           content = "",
           headers=structure(list(
             'content-type'="text/csv; charset=utf-8",
             'location'=url
           ),
           class = c("insensitive", "list")),
      class = "response")
    )
    
    redirectCall <- TRUE
    stub(makeApiCall, ".curlPost", function(...)
      if(redirectCall) { redirectCall <<- FALSE; redirect  } else 
                       { redcapAPI:::.curlPost(...) }
    )
    
    expect_warning(
      response <- makeApiCall(rcon, 
                        body = list(content = "version", 
                                    format = "csv")),
      paste0("Permanent 301 redirect https://test.xyz/api to ", url)
    )
    
    expect_equal(response$status_code, 200L)
  }
)

test_that(
  "makeApiCall handles temporary redirect",
  {
    local_reproducible_output(width = 200)

    h <- new_handle(timeout = 1L)
    redirect <- structure(
      list(url = "https://test.xyz/api",
           status_code = 302L,
           content = "",
           headers=structure(list(
             'content-type'="text/csv; charset=utf-8",
             'location'=url
           ),
           class = c("insensitive", "list")),
      class = "response")
    )
    
    redirectCall <- TRUE
    stub(makeApiCall, ".curlPost", function(...)
      if(redirectCall) { redirectCall <<- FALSE; redirect  } else 
                       { redcapAPI:::.curlPost(...) }
    )
   
    expect_message(
      response <- makeApiCall(rcon, 
                        body = list(content = "version", 
                                    format = "csv")),
      paste0("Temporary 302 redirect https://test.xyz/api to ", url)
    )
    
    expect_equal(response$status_code, 200L)
  }
)
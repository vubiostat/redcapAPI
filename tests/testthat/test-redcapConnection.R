context("redcapConnection")

test_that("redcapApiConnection can be created",
          expect_equal(
            class(redcapConnection(url = url, token = API_KEY)),
            "redcapApiConnection"
          )
)

test_that("redcapConnection throws an error if url is missing",
          expect_error(redcapConnection(token = API_KEY))
)

test_that("redcapConnection throws an  error if token is missing",
          expect_error(redcapConnection(url = url))
)

test_that(
  "return an error if retries is not integerish(1) > 0", 
  {
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

# Caching tests -----------------------------------------------------

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Caching of metadata", 
  {
    rcon$flush_all()
    expect_false(rcon$has_metadata())
    
    rcon$metadata()
    expect_true(rcon$has_metadata())
    
    rcon$flush_metadata()
    expect_false(rcon$has_metadata())
    
    rcon$refresh_metadata()
    expect_true(rcon$has_metadata())
  }
)

test_that(
  "caching of arms", 
  {
    rcon$flush_all()
    expect_false(rcon$has_arms())
    
    rcon$arms()
    expect_true(rcon$has_arms())
    
    rcon$flush_arms()
    expect_false(rcon$has_arms())
    
    rcon$refresh_arms()
    expect_true(rcon$has_arms())
  }
)

test_that(
  "caching of events", 
  {
    rcon$flush_all()
    expect_false(rcon$has_events())
    
    rcon$events()
    expect_true(rcon$has_events())
    
    rcon$flush_events()
    expect_false(rcon$has_events())
    
    rcon$refresh_events()
    expect_true(rcon$has_events())
  }
)

test_that(
  "caching of fieldnames", 
  {
    rcon$flush_all()
    expect_false(rcon$has_fieldnames())
    
    rcon$fieldnames()
    expect_true(rcon$has_fieldnames())
    
    rcon$flush_fieldnames()
    expect_false(rcon$has_fieldnames())
    
    rcon$refresh_fieldnames()
    expect_true(rcon$has_fieldnames())
  }
)

test_that(
  "caching of mapping", 
  {
    rcon$flush_all()
    expect_false(rcon$has_mapping())
    
    rcon$mapping()
    expect_true(rcon$has_mapping())
    
    rcon$flush_mapping()
    expect_false(rcon$has_mapping())
    
    rcon$refresh_mapping()
    expect_true(rcon$has_mapping())
  }
)

test_that(
  "caching of users", 
  {
    rcon$flush_all()
    expect_false(rcon$has_users())
    
    rcon$users()
    expect_true(rcon$has_users())
    
    rcon$flush_users()
    expect_false(rcon$has_users())
    
    rcon$refresh_users()
    expect_true(rcon$has_users())
  }
)

test_that(
  "caching of version", 
  {
    rcon$flush_all()
    expect_false(rcon$has_version())
    
    rcon$version()
    expect_true(rcon$has_version())
    
    rcon$flush_version()
    expect_false(rcon$has_version())
    
    rcon$refresh_version()
    expect_true(rcon$has_version())
  }
)

test_that(
  "caching of projectInformation", 
  {
    rcon$flush_all()
    expect_false(rcon$has_projectInformation())
    
    rcon$projectInformation()
    expect_true(rcon$has_projectInformation())
    
    rcon$flush_projectInformation()
    expect_false(rcon$has_projectInformation())
    
    rcon$refresh_projectInformation()
    expect_true(rcon$has_projectInformation())
  }
)

test_that(
  "caching of instruments", 
  {
    rcon$flush_all()
    expect_false(rcon$has_instruments())
    
    rcon$instruments()
    expect_true(rcon$has_instruments())
    
    rcon$flush_instruments()
    expect_false(rcon$has_instruments())
    
    rcon$refresh_instruments()
    expect_true(rcon$has_instruments())
  }
)

test_that(
  "caching of fileRepository", 
  {
    rcon$flush_all()
    expect_false(rcon$has_fileRepository())
    
    rcon$fileRepository()
    expect_true(rcon$has_fileRepository())
    
    rcon$flush_fileRepository()
    expect_false(rcon$has_fileRepository())
    
    rcon$refresh_fileRepository()
    expect_true(rcon$has_fileRepository())
  }
)

test_that(
  "retrieve and set retries", 
  {
    expect_equal(rcon$retries(), 5)
    expect_error(rcon$set_retries("3"))
    
    rcon$set_retries(4)
    expect_equal(rcon$retries(), 4)
    rcon$set_retries(5)
  }
)

test_that(
  "retrieve and set retry_interval", 
  {
    expect_equal(rcon$retry_interval(), 
                 c(2^(1:5)))
    
    expect_error(rcon$set_retry_interval(-3))
    
    rcon$set_retry_interval(3)
    expect_equal(rcon$retry_interval(), 
                 rep(3, 5))
    
    rcon$set_retry_interval(2^(1:5))
  }
)

test_that(
  "retrieve and set retry_quietly", 
  {
    expect_true(rcon$retry_quietly())
    
    expect_error(rcon$set_retry_quietly("FALSE"))
    
    rcon$set_retry_quietly(FALSE)
    expect_false(rcon$retry_quietly())
    
    rcon$set_retry_quietly(TRUE)
  }
)

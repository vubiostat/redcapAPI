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



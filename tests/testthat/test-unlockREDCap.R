context("unlockREDCap")

test_that(
  ".connectAndCheck returns result of redcapConnection",
  {
    mockery::stub(.connectAndCheck, "redcapConnection", list(metadata=function(...) TRUE))
  
    expect_true(.connectAndCheck("key", "url")$metadata())
  }
)

test_that(
  ".connectAndCheck returns NULL on a 403 (Invalid KEY)",
  {
    with_mock(redcapConnection = mockery::mock(stop("403")),
              .env = "redcapAPI",
              expect_true(is.null(.connectAndCheck("key", "url")))
    )
    #expect_true(is.null(.connectAndCheck("ASDF", url)))
  }
)

test_that(
  ".connectAndCheck errors with bad url",
  expect_error(.connectAndCheck("key", "badurl"), "Unable to connect")
)

test_that(
  ".unlockYamlOverride return empty list when override yaml doesn't exist",
  {
    mockery::stub(.unlockYamlOverride, "file.exists", FALSE)
    
    x <- .unlockYamlOverride("TestRedcapAPI", url)
    expect_class(x, "list")
    expect_true(length(x) == 0)
  }
)

test_that(
  ".unlockYamlOverride stops if no redcapAPI entry is found",
  {
    mockery::stub(.unlockYamlOverride, "file.exists", TRUE)
    mockery::stub(.unlockYamlOverride, "read_yaml", list())
    
    expect_error(.unlockYamlOverride("TestRedcapAPI", url),
                 "does not contain required 'redcapAPI' entry")
  }
)

test_that(
  ".unlockYamlOverride stops if no redcapAPI$keys entry is found",
  {
    mockery::stub(.unlockYamlOverride, "file.exists", TRUE)
    mockery::stub(.unlockYamlOverride, "read_yaml", list(redcapAPI=list()))
    
    expect_error(.unlockYamlOverride("TestRedcapAPI", url),
                 "does not contain required 'keys' entry")
  }
)

test_that(
  ".unlockYamlOverride returns an entry for every connection",
  {
    mockery::stub(.unlockYamlOverride, "file.exists", TRUE)
    mockery::stub(.unlockYamlOverride, "read_yaml",
                  list(redcapAPI=list(keys=list(TestRedcapAPI='xyz', Sandbox='xyz'))))
    mockery::stub(.unlockYamlOverride, ".connectAndCheck", TRUE)
    x <- .unlockYamlOverride(c("TestRedcapAPI", "Sandbox"), url)
    expect_true(x$TestRedcapAPI)
    expect_true(x$Sandbox)
  }
)

test_that(
  ".unlockYamlOverride returns an entry for every connection renamed as requested",
  {
    mockery::stub(.unlockYamlOverride, "file.exists", TRUE)
    mockery::stub(.unlockYamlOverride, "read_yaml",
                  list(redcapAPI=list(keys=list(TestRedcapAPI='xyz', Sandbox='xyz'))))
    mockery::stub(.unlockYamlOverride, ".connectAndCheck", TRUE)
    x <- .unlockYamlOverride(c(rcon="TestRedcapAPI", sand="Sandbox"), url)
    expect_true(x$rcon)
    expect_true(x$sand)
  }
)

context("unlockREDCap .connectAndCheck")

test_that(
  ".connectAndCheck returns result of redcapConnection",
  {
    mockery::stub(.connectAndCheck, "redcapConnection", list(metadata=function(...) TRUE))
  
    expect_true(.connectAndCheck("key", "url")$metadata())
  }
)

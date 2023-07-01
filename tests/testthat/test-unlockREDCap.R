context("unlockREDCap")

test_that(
  "removes existing variables from environment", 
  {
    mockery::stub(unlockREDCap, "file.exists", TRUE) 
    mockery::stub(unlockREDCap, "read_yaml", NULL)
    
    yo <- new.env()
    assign("xyz", 3, envir=yo)
    assign("abc", 2, envir=yo)
    assign("ijk", 1, envir=yo)
    
    expect_error(unlockREDCap(c("abc", "xyz"), url="url", keyring="keyring", envir=yo))
    expect_false(exists("xyz", envir=yo))
    expect_false(exists("abc", envir=yo))
    expect_true(exists("ijk", envir=yo))
  }
)
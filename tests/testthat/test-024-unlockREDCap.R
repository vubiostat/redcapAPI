context("unlockREDCap")

library(mockery)

test_that(
  ".connectAndCheck returns result of redcapConnection",
  {
    stub(.connectAndCheck, "redcapConnection", list(metadata=function(...) TRUE))
  
    expect_true(.connectAndCheck("key", "url")$metadata())
  }
)

test_that(
  ".connectAndCheck returns NULL on a 403 (Invalid KEY)",
  {
    stub(.connectAndCheck, "redcapConnection",
                  mock(stop("403")))
    expect_true(is.null(.connectAndCheck("key", "url")))
  }
)

test_that(
  ".connectAndCheck errors with bad url",
  expect_error(.connectAndCheck("key", "badurl"), "Unable to connect")
)

test_that(
  ".unlockYamlOverride return empty list when override yaml doesn't exist",
  {
    stub(.unlockYamlOverride, "file.exists", FALSE)
    
    x <- .unlockYamlOverride("TestRedcapAPI", url)
    expect_class(x, "list")
    expect_true(length(x) == 0)
  }
)

test_that(
  ".unlockYamlOverride stops if no redcapAPI entry is found",
  {
    stub(.unlockYamlOverride, "file.exists", TRUE)
    stub(.unlockYamlOverride, "read_yaml", list())
    
    expect_error(.unlockYamlOverride("TestRedcapAPI", url),
                 "does not contain required 'redcapAPI' entry")
  }
)

test_that(
  ".unlockYamlOverride stops if no redcapAPI$keys entry is found",
  {
    stub(.unlockYamlOverride, "file.exists", TRUE)
    stub(.unlockYamlOverride, "read_yaml", list(redcapAPI=list()))
    stub(.unlockYamlOverride, ".connectAndCheck", TRUE)
    
    expect_error(.unlockYamlOverride("TestRedcapAPI", url),
                 "does not contain required 'keys' entry")
  }
)

test_that(
  ".unlockYamlOverride stops if a list redcapAPI$keys entry is found",
  {
    stub(.unlockYamlOverride, "file.exists", TRUE)
    stub(.unlockYamlOverride, "read_yaml", list(redcapAPI=list(keys=list(TestRedcapAPI=list()))))
    stub(.unlockYamlOverride, ".connectAndCheck", TRUE)
    
    expect_error(.unlockYamlOverride("TestRedcapAPI", url),
                 "does not have API_KEY for")
  }
)

test_that(
  ".unlockYamlOverride stops if a non string redcapAPI$keys entry is found",
  {
    stub(.unlockYamlOverride, "file.exists", TRUE)
    stub(.unlockYamlOverride, "read_yaml", list(redcapAPI=list(keys=list(TestRedcapAPI=TRUE))))
    stub(.unlockYamlOverride, ".connectAndCheck", TRUE)
    
    expect_error(.unlockYamlOverride("TestRedcapAPI", url),
                 "invalid entry")
  }
)


test_that(
  ".unlockYamlOverride returns an entry for every connection",
  {
    stub(.unlockYamlOverride, "file.exists", TRUE)
    stub(.unlockYamlOverride, "read_yaml",
                  list(redcapAPI=list(keys=list(TestRedcapAPI='xyz', Sandbox='xyz'))))
    stub(.unlockYamlOverride, ".connectAndCheck", TRUE)
    x <- .unlockYamlOverride(c("TestRedcapAPI", "Sandbox"), url)
    expect_true(x$TestRedcapAPI)
    expect_true(x$Sandbox)
  }
)

test_that(
  ".unlockYamlOverride returns an entry for every connection renamed as requested",
  {
    stub(.unlockYamlOverride, "file.exists", TRUE)
    stub(.unlockYamlOverride, "read_yaml",
                  list(redcapAPI=list(keys=list(TestRedcapAPI='xyz', Sandbox='xyz'))))
    stub(.unlockYamlOverride, ".connectAndCheck", TRUE)
    x <- .unlockYamlOverride(c(rcon="TestRedcapAPI", sand="Sandbox"), url)
    expect_true(x$rcon)
    expect_true(x$sand)
  }
)

test_that(
  ".unlockKeyring pulls password from env and writes back",
  {
    stub(.unlockKeyring, "keyring_list", 
                  data.frame(keyring=c("Elsewhere", "API_KEYs", "JoesGarage"),
                             num_secrets=0:2,
                             locked=rep(TRUE, 3)))
    stub(.unlockKeyring, ".getPWGlobalEnv", "xyz")
    stub(.unlockKeyring, "keyring_unlock", NULL)

    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1}
    
    .unlockKeyring("API_KEYs", passwordFUN)
    
    expect_true(calls == 0) # No requests for password from user
    expect_true(Sys.getenv("REDCAPAPI_PW") == "xyz")
  }
)

test_that(
  ".unlockKeyring asks user for password when not in env, unlocks and writes to env",
  {
    stub(.unlockKeyring, "keyring_list", 
                  data.frame(keyring=c("Elsewhere", "API_KEYs", "JoesGarage"),
                             num_secrets=0:2,
                             locked=rep(TRUE, 3)))
    stub(.unlockKeyring, ".getPWGlobalEnv", "")
    stub(.unlockKeyring, "keyring_unlock", NULL)
    
    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1; "xyz"}
    
    .unlockKeyring("API_KEYs", passwordFUN)
    
    expect_true(calls == 1) # Requests password
    expect_true(Sys.getenv("REDCAPAPI_PW") == "xyz")
  }
)

test_that(
  ".unlockKeyring asks user for password and aborts when they cancel",
  {
    stub(.unlockKeyring, "keyring_list", 
                  data.frame(keyring=c("Elsewhere", "API_KEYs", "JoesGarage"),
                             num_secrets=0:2,
                             locked=rep(TRUE, 3)))
    stub(.unlockKeyring, ".getPWGlobalEnv", "")
    stub(.unlockKeyring, "keyring_unlock", NULL)
    
    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1; ""}
    
    expect_error(.unlockKeyring("API_KEYs", passwordFUN), "User aborted keyring")
    
    expect_true(calls == 1) # Requests password
  }
)


test_that(
  ".unlockKeyring asks user for password when one in env fails, unlocks and writes to env",
  {
    stub(.unlockKeyring, "keyring_list", 
                  data.frame(keyring=c("Elsewhere", "API_KEYs", "JoesGarage"),
                             num_secrets=0:2,
                             locked=rep(TRUE, 3)))
    stub(.unlockKeyring, "Sys.getenv", 
                  mock("fail", ""))
    stub(.unlockKeyring, "keyring_unlock",
                  mock(stop("fail"), "joe"))
    
    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1; "xyz"}
    
    .unlockKeyring("API_KEYs", passwordFUN)
    
    expect_true(calls == 1) # Requests password
    expect_true(Sys.getenv("REDCAPAPI_PW") == "xyz")
  }
)

test_that(
  ".unlockKeyring creates keyring if it doesn't exist",
  {
#    skip_if(TRUE, 
#            "At the time of writing, testthat mock framework not working in all environments")
    
    Sys.unsetenv("REDCAPAPI_PW")
    stub(.unlockKeyring, "keyring_list",
                  data.frame(keyring=c("Elsewhere", "API_KEYs", "JoesGarage"),
                             num_secrets=0:2,
                             locked=rep(TRUE, 3)))
    m <- mock(TRUE)
  
    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1; "xyz"}
    
    with_mocked_bindings(
      {
        .unlockKeyring("MakeMe", passwordFUN)
        expect_call(m, 1, keyring_create(keyring,password))
      },
      keyring_create = m
    )
    
    expect_equal(mock_args(m)[[1]], list("MakeMe", "xyz"))
    expect_true(calls == 1) # Asks user for password
    expect_true(Sys.getenv("REDCAPAPI_PW") == "xyz") # Stores result
  }
)


test_that(
  ".unlockKeyring creates keyring respects user cancel",
  {
#    skip_if(TRUE, 
#            "At the time of writing, testthat mock framework not working in all environments")
    
    Sys.unsetenv("REDCAPAPI_PW")
    stub(.unlockKeyring, "keyring_list", 
         data.frame(keyring=c("Elsewhere", "API_KEYs", "JoesGarage"),
                    num_secrets=0:2,
                    locked=rep(TRUE, 3)))
    m <- mock(TRUE)
    
    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1; ""}
    
    with_mocked_bindings(
      {
        expect_error(.unlockKeyring("MakeMe", passwordFUN), "User cancelled")
        expect_called(m, 0)
      },
      keyring_create = m
    )
    
    expect_true(calls == 1) # Asks user for password
    expect_true(Sys.getenv("REDCAPAPI_PW") == "") # Nothing Stored
  }
)

test_that(
  "unlockREDCap pulls API_KEY and opens connection from keyring returning as list",
  {
    skip_if(Sys.getenv("CI") == "1",
            "CI cannot test user interactions")
    stub(unlockREDCap, ".unlockYamlOverride", list()) # No yaml
 
    expect_silent(x <- unlockREDCap(c(rcon="TestRedcapAPI"), url, keyring="API_KEYs"))
    expect_true("rcon" %in% names(x))
    expect_class(x$rcon, "redcapApiConnection")
  }
)

test_that(
  "unlockREDCap pulls API_KEY and opens connection from keyring written to env",
  {
    skip_if(Sys.getenv("CI") == "1",
            "CI cannot test user interactions")
    
    stub(unlockREDCap, ".unlockYamlOverride", list()) # No yaml
    
    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1; ""}
    e <- new.env()
    
    expect_silent(unlockREDCap(
      c(rcon="TestRedcapAPI"), url, keyring="API_KEYs",
      envir=e, passwordFUN=passwordFUN))
    
    expect_true(exists("rcon", envir=e))
    expect_class(e[["rcon"]], "redcapApiConnection")
    expect_true(calls == 0) # No password requests
  }
)

test_that(
  "unlockREDCap asks for API_KEY if not stored, opens connection and stores",
  {
    skip_if(Sys.getenv("CI") == "1",
            "CI cannot test user interactions")
    
    stub(unlockREDCap, ".unlockYamlOverride", list()) # No yaml
    
    stub(unlockREDCap, "key_list",
         data.frame(service="recapAPI", username="Nadda"))
    
    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1; "xyz"}
    
    m <- mock(TRUE)
    n <- mock(TRUE)

    with_mocked_bindings(
      {
        x <- unlockREDCap(
          c(rcon="George"), url, keyring="API_KEYs",
          passwordFUN=passwordFUN)
      },
      key_set_with_value = m,
      .connectAndCheck = n
    )

    expect_true("rcon" %in% names(x))
    expect_true(x$rcon)
    expect_true(calls == 1) # Called to ask once
    expect_called(m, 1) # Called key_set_with_value once
    #print(mock_args(m)[[1]])
    expect_equal(mock_args(m)[[1]], list(service="redcapAPI", username="George", password="xyz", keyring="API_KEYs"))
    expect_called(n, 1) # Called .connectAndCheck
  }
)
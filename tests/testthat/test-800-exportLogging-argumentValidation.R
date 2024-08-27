context("Export Logging Argument Validation")

#####################################################################
# Argument Validation 

test_that(
  "Return an error when rcon is not a redcapConnection object", 
  {
    local_reproducible_output(width = 200)
    expect_error(
      exportLogging(rcon = "not an rcon"), 
      "no applicable method for 'exportLogging'"
    )
  }
)

test_that(
  "Return an error when batchInterval is given without beginTime",
  expect_error(
    exportLogging(rcon, batchInterval=7),
    "Variable 'beginTime': Must have length >= 1, but has length 0"
  )
)


test_that(
  "Return an error when log_type is not one of event types",
  {
    local_reproducible_output(width = 200)
    expect_error(
      exportLogging(rcon, 
                    logtype = "abc"), 
      "Variable 'logtype': Must be element of set"
    )
  }
)


test_that(
  "Return an error when user is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(
      exportLogging(rcon, 
                    user = 1:2), 
      "Variable 'user': Must be of type 'character'"
    )
  }
)

test_that(
  "Return an error when record is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(
      exportLogging(rcon, 
                    record = 1:2), 
      "Variable 'record': Must be of type 'character'"
    )
  }
)


test_that(
  "Return an error when dag is not a character", 
  {
    local_reproducible_output(width = 200)
    expect_error(
      exportLogging(rcon, 
                    dag = 1:2), 
      "Variable 'dag': Must be of type 'character'"
    )
  }
)


test_that(
  "Return an error when beginTime is not POSIXct", 
  {
    local_reproducible_output(width = 200)
    expect_error(
      exportLogging(rcon, 
                    beginTime = "2023-01-01 00:00:00"), 
      "Variable 'beginTime': Must be of type 'POSIXct'"
    )
  }
)


test_that(
  "Return an error when beginTime has length > 1", 
  {
    local_reproducible_output(width = 200)
    expect_error(
      exportLogging(rcon, 
                    beginTime = rep(Sys.time(), 2)), 
      "Variable 'beginTime': Must have length <= 1"
    )
  }
)


test_that(
  "Return an error when endTime is not POSIXct", 
  {
    local_reproducible_output(width = 200)
    expect_error(
      exportLogging(rcon, 
                    endTime = "2023-01-01 00:00:00"), 
      "Variable 'endTime': Must be of type 'POSIXct'"
    )
  }
)


test_that(
  "Return an error when endTime has length > 1", 
  {
    local_reproducible_output(width = 200)
    expect_error(
      exportLogging(rcon, 
                    endTime = rep(Sys.time(), 2)), 
      "Variable 'endTime': Must have length <= 1"
    )
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)

    expect_error(exportLogging(rcon, 
                               config = list(1)), 
                 "'config': Must have names")
    expect_error(exportLogging(rcon, 
                               config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportLogging(rcon, 
                               api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportLogging(rcon, 
                               api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

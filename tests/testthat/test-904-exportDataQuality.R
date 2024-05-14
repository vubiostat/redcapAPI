context("exportDataQuality.R")

prefix <- 'vanderbilt_dataQuality'

test_that("Data queries can be exported",{
  skip_if(!RUN_DATAQUALITY_TEST, 
        "No Data Quality Project provided. Tests Skipped")
  
  dq <- exportDataQuality(dqrcon, prefix)
  expect_gte(length(dq), 1)
})

test_that(
  "Return error messages if Data Quality Module not enabled",
  {
    expect_error(exportDataQuality(rcon, prefix), 
                 "Error in result: Make sure the Data Quality API module is enabled in your project.")
  }
)


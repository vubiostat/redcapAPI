context("Initialize Test Project")

test_that("Project Can Purged",{
  expect_no_error(purgeProject(rcon, purge_all = TRUE))
})

test_that("Metadata can be imported",{
  expect_no_error(
    load(test_path("testdata", "test_redcapAPI_MetaData.Rdata"))
  )
  
  expect_no_error(importMetaData(rcon, test_redcapAPI_MetaData[1, ]))
})

test_that("Metadata with empty strings can be imported",
{
  expect_no_error(
    load(test_path("testdata", "test_redcapAPI_MetaData.Rdata"))
  )
  expect_no_error(purgeProject(rcon, purge_all = TRUE))
  data <- test_redcapAPI_MetaData[1,]
  data[is.na(data)] <- ''
  expect_no_error(importMetaData(rcon, data))
})
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
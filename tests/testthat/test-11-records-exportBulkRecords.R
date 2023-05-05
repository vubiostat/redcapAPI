context("exportBulkRecords")

test_that("Requires Connection",{
  expect_error(exportBulkRecords(), "rcon")
  expect_error(exportBulkRecords(FALSE), "rcon")
})

test_that("Connections must be named",{
  expect_error(exportBulkRecords(rcon), "rcon")
})

test_that("Form names must match rcon",{
  expect_error(exportBulkRecords(list("x"=rcon), list("y"=c("joe"))),
               "subset")
})

test_that("envir must be an environment",{
  expect_error(exportBulkRecords(list("x"=rcon), envir=TRUE), "envir")
})

test_that("envir must be an environment",{
  expect_error(exportBulkRecords(list("x"=rcon), envir=TRUE), "envir")
})

test_that("sep must be a single string",{
  expect_error(exportBulkRecords(list("x"=rcon), sep=TRUE), "sep")
  expect_error(exportBulkRecords(list("x"=rcon), sep=NULL), "sep")
  expect_error(exportBulkRecords(list("x"=rcon), sep=c("a", "b")), "sep")
})

test_that("post must be a 2 argument function",{
  expect_error(exportBulkRecords(list("x"=rcon), post=TRUE), "post")
  expect_error(exportBulkRecords(list("x"=rcon), post=function(x) 1), "post")
  expect_error(exportBulkRecords(list("x"=rcon), post=function(x,y,z) 1), "post")
})

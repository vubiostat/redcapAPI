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

test_that("returns list of records",{
  Recs <- exportBulkRecords(list("test"=rcon))
  expect_list(Recs)
  for(i in rcon$instruments()$instrument_name) 
    expect_true(paste0("test_", i) %in% names(Recs))
  expect_data_frame(Recs[["test_fieldtovar_datetimes"]])
})

test_that("returns list of records respecting NA override for forms",{
  Recs <- exportBulkRecords(list("test"=rcon), forms=list(test=NA))
  expect_list(Recs)
  expect_true("test" == names(Recs))
  expect_data_frame(Recs[["test"]])
})

test_that("returns list of forms with proper separator",{
  Recs <- exportBulkRecords(list(test=rcon),
                            list(test=c("fieldtovar_datetimes","randomization")),
                            sep=".")
  expect_list(Recs)
  expect_true(all(names(Recs) == c("test.fieldtovar_datetimes","test.randomization")))
  expect_data_frame(Recs[["test.fieldtovar_datetimes"]])
  expect_data_frame(Recs[["test.randomization"]])
})

test_that("assign to environment records",{
  exportBulkRecords(list("test"=rcon), envir=environment())
  expect_data_frame(test_fieldtovar_datetimes)
})

test_that("assign to environment number records",{
  exportBulkRecords(list("test"=rcon), envir=1)
  expect_data_frame(test_fieldtovar_datetimes)
})

test_that("assigns forms with proper separator",{
  exportBulkRecords(list(test=rcon),
                    list(test=c("fieldtovar_datetimes","randomization")),
                    envir=environment())
  expect_data_frame(test_fieldtovar_datetimes)
  expect_data_frame(test_randomization)
})

test_that("calls post",{
  Recs <- exportBulkRecords(list(test=rcon),
                            post=function(recs,rcon) {"x"}
                            )
  expect_true(Recs[["test_fieldtovar_datetimes"]] == "x")

  Recs <- exportBulkRecords(list(test=rcon),
                            list(test=c("fieldtovar_datetimes","randomization")),
                            post=function(recs,rcon) {"y"})
  expect_true(Recs[["test_fieldtovar_datetimes"]] == "y")
  expect_true(Recs[["test_randomization"]] == "y")
  
})

test_that("Fails for logical forms",{
  expect_error(exportBulkRecords(list("x"=rcon), list("x"=TRUE)))
  expect_error(exportBulkRecords(list("x"=rcon), list("x"=FALSE)))
  expect_error(exportBulkRecords(list("x"=rcon), list("x"=c(NA, NA))))
})


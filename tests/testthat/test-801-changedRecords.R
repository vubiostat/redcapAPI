context("changedRecords Functionality")

test_that(
  "Return an error when rcon is not a redcapConnection object",
  {
    local_reproducible_output(width = 200)
    expect_error(
      changedRecords(rcon = "not an rcon"),
      "no applicable method for 'exportLogging'"
    )
  }
)

test_that(
  "Returns records known to change in prior tests",
  {
    recs <- as.character(1:20)
    x <- changedRecords(rcon, beginTime=as.POSIXct(Sys.time()-86400))

    expect_true(all(recs %in% x$created))
    expect_true(all(recs %in% x$updated))
    expect_true(all(as.character(1:3) %in% x$deleted))

  }
)

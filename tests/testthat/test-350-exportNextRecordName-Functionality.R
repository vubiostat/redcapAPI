context("Export Next Record Name Functionality")

test_that(
  "Return the next record name", 
  {
    Records <- exportRecords(rcon, 
                             fields = "record_id")
    next_id <- max(as.numeric(Records$record_id)) + 1
    # Since the test data base can be expected to 
    expect_equal(exportNextRecordName(rcon), 
                 next_id)
  }
)

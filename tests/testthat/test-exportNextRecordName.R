context("exportNextRecordName.R")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportNextRecordName("not an rcon"), 
                 "no applicable method for 'exportNextRecordName'")
  }
)

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


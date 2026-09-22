

test_that(
  "Return the next record name",
  {
    Records <- exportRecordsTyped(rcon, fields = "record_id")
    next_id <- exportNextRecordName(rcon)
    importRecords(rcon, data.frame(record_id=next_id, record_id_complete="2"))
    Records <- exportRecordsTyped(rcon, fields = "record_id")
    expect_contains(Records$record_id, next_id);
  }
)

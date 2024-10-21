context("Rename Record Functionality")

test_that(
  "Rename a record", 
  {
    expect_true(renameRecord(rcon, 
                             record_name = "1", 
                             new_record_name = "100"))
    
    Rec <- exportRecordsTyped(rcon, 
                              fields = "record_id")
    
    expect_true("100" %in% Rec$record_id)
    
    expect_true(renameRecord(rcon, 
                             record_name = "100", 
                             new_record_name = "1"))
    
    Rec <- exportRecordsTyped(rcon, 
                              fields = "record_id")
    
    expect_false("100" %in% Rec$record_id)
  }
)


test_that(
  "Rename multiple records", 
  {
    expect_equal(renameRecord(rcon, 
                              record_name = c("1", "2"), 
                              new_record_name = c("101", "102")), 
                 c(TRUE, TRUE))
    
    expect_equal(renameRecord(rcon, 
                              record_name = c("101", "102"), 
                              new_record_name = c("1", "2")), 
                 c(TRUE, TRUE))
  }
)

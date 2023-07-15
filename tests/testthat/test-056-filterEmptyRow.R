context("filterEmptyRow Functionality")

test_that(
  "Argument Validation", 
  {
    local_reproducible_output(width = 200
                              )
    expect_error(filterEmptyRow("not a data frame", 
                                rcon), 
                 "'data': Must be of type 'data.frame'")
    
    expect_error(filterEmptyRow(mtcars, 
                                "not an rcon"), 
                 "'rcon': Must inherit from class 'redcapConnection'")
  }
)


test_that(
  "Remove empty rows", 
  {
    TestThis <- data.frame(record_id = 1:4, 
                           redcap_event_name = sprintf("event_%s_arm_1", 
                                                       1:4), 
                           value = c("this", NA, "that", NA))
    
    Cleansed <- filterEmptyRow(TestThis, rcon)
    
    expect_data_frame(Cleansed, 
                      ncols = 3, 
                      nrows = 2)
    
    expect_equal(Cleansed$record_id, 
                 c(1, 3))
    
    expect_equal(Cleansed$value, 
                 c("this", "that"))
    
    TestAnother <- data.frame(record_id = 1:4, 
                              redcap_event_name = sprintf("event_%s_arm_1", 
                                                          1:4), 
                              value = c("this", NA, "that", NA), 
                              value2 = c("only", "one", "missing", NA))
    
    Cleansed <- filterEmptyRow(TestAnother, rcon)
    
    expect_data_frame(Cleansed, 
                      ncols = 4, 
                      nrows = 3)
    
    expect_equal(Cleansed$record_id, 
                 c(1, 2, 3))
    
    expect_equal(Cleansed$value, 
                 c("this", NA, "that"))
    
    expect_equal(Cleansed$value2, 
                 c("only", "one", "missing"))
  }
)

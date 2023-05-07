context("writeDataForImport.R")

test_that(
  "Convert data frames to text", 
  {
    data1 <- data.frame(arm_num = 1:3, 
                        name = c("Arm1", "Arm2", "Arm3"))
    data2 <- data.frame(record_id = 1:2, 
                        date_dmy = rep(Sys.Date(), 2))
    
    expect_equal(writeDataForImport(data1), 
                 "\"arm_num\",\"name\"\n1,\"Arm1\"\n2,\"Arm2\"\n3,\"Arm3\"")
    
    expect_equal(writeDataForImport(data2), 
                 sprintf("\"record_id\",\"date_dmy\"\n1,%s\n2,%s", 
                         format(Sys.Date()), 
                         format(Sys.Date())))
  }
)

#####################################################################
# Argument Validation

test_that(
  "Return an error when data is not a data frame", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(writeDataForImport(data = "not a data frame"), 
                 "'data': Must be of type 'data.frame'")
  }
)

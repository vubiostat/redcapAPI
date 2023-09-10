context("reviewInvalidRecords")

#####################################################################
# Argument Validation                                            ####

test_that(
  "Return an error if data is not a data.frame", 
  {
    expect_error(reviewInvalidRecords("not data"), 
                 "'data': Must be of type 'data.frame'")
  }
)

test_that(
  "Return an error if quiet is not logical(1)", 
  {
    expect_error(reviewInvalidRecords(mtcars, 
                                      quiet = "TRUE"), 
                 "'quiet': Must be of type 'logical'")
    
    expect_error(reviewInvalidRecords(mtcars, 
                                      quiet = c(TRUE, FALSE)), 
                 "'quiet': Must have length 1")
  }
)

#####################################################################
# Functionality                                                  ####

test_that(
  "Return the invalid attribute", 
  {
    TheseData <- mtcars
    Invalid <- data.frame(row = numeric(0), 
                          record_id = character(0), 
                          field_name = character(0), 
                          field_type = character(0), 
                          value = character(0), 
                          stringsAsFactors = FALSE)
    
    class(Invalid) <- c("invalid", "data.frame")
    attr(Invalid, "time") <- format(Sys.Date(), "%c")
    attr(Invalid, "version") <- "13.0.0"
    attr(Invalid, "project") <- "Project title"
    
    attr(TheseData, "invalid") <- Invalid
    
    expect_data_frame(reviewInvalidRecords(TheseData))
    
  }
)

test_that(
  "Return NULL if the invalid attribute is not present", 
  {
    expect_null(reviewInvalidRecords(mtcars))
    expect_message(reviewInvalidRecords(mtcars, quiet = FALSE), 
                   "does not have an 'invalid' attribute.")
  }
)

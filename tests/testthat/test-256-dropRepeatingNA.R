context("dropRepeatingNA.R")

#####################################################################
# Argument Validation                                            ####

test_that(
  "Return an error if Records is not a data frame", 
  {
    local_reproducible_output(width = 200)
    expect_error(dropRepeatingNA("not an error", 
                                 rcon), 
                 "'Records': Must be of type 'data.frame'")
  }
)

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(dropRepeatingNA(mtcars, 
                                 "not an rcon"), 
                 "'rcon': Must inherit from class 'redcapConnection'")
  }
)

test_that(
  "Return an error if quiet is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(dropRepeatingNA(mtcars, 
                                 rcon, 
                                 quiet = "TRUE"), 
                 "'quiet': Must be of type 'logical'")
    
    expect_error(dropRepeatingNA(mtcars, 
                                 rcon, 
                                 quiet = c(TRUE, FALSE)), 
                 "'quiet': Must have length 1")
  }
)

#####################################################################
# Functionality                                                  ####

test_that(
  "Drop missing values from repeating instrument", 
  {
    TheseData <- data.frame(record_id = c(1, 1, 1, 2, 2, 3), 
                            redcap_repeat_instrument = c("instrument1", 
                                                         NA_character_, 
                                                         "instrument_1", 
                                                         "instrument_1", 
                                                         "instrument_2", 
                                                         NA_character_), 
                            redcap_repeat_instance = c(1, 
                                                       NA_real_, 
                                                       2, 
                                                       1, 
                                                       1, 
                                                       NA_real_))
    
    expect_message(dropRepeatingNA(TheseData, rcon), 
                   "Project .+ had 6 rows, subsetted to 4 rows")
    
    expect_no_message(dropRepeatingNA(TheseData, rcon, TRUE))
    
    expect_data_frame(dropRepeatingNA(TheseData, rcon, quiet = TRUE), 
                      nrows = 4, 
                      ncols = 3)
  }
)

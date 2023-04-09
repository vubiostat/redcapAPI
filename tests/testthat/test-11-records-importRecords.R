context("importRecords Functionality")

rcon <- redcapConnection(url = url, token = API_KEY)

rec <- exportRecords(rcon, mChoice=FALSE)
rows <- nrow(rec)

NewRecords <- rbind(rec[1,], rec[1,])
NewRecords$record_id <- c("delete.me", "delete.too")

#####################################################################
# Tests

test_that(
  "importRecords overwriteBehavior", 
  {
    # Import new records for the test
    importRecords(rcon, 
                  data = NewRecords)
    
    # Change a value in the new record
    NewNewRecords <- NewRecords
    NewNewRecords$date_dmy <- ""
    
    # overwriteBehavior = normal produces no change
    
    importRecords(rcon, 
                  data = NewNewRecords, 
                  overwriteBehavior = "normal")
    
    suppressWarnings({
      Compare <- exportRecords(rcon, 
                               records = NewNewRecords$record_id, 
                               fields = "date_dmy")
    })
      
    expect_true(all(!is.na(Compare$date_dmy)))
    
    # overwriteBehavior = overwrite produces a change
    
    importRecords(rcon, 
                  data = NewNewRecords, 
                  overwriteBehavior = "overwrite")
    
    suppressWarnings({
      Compare <- exportRecords(rcon, 
                               records = NewNewRecords$record_id, 
                               fields = "date_dmy")
    })
    
    expect_true(all(is.na(Compare$date_dmy)))
    
    # Cleanup 
    deleteRecords(rcon, 
                  records = NewRecords$record_id)
  }
)

test_that(
  "importRecords returnContent", 
  {
    # Import new records returnContent = count returns count of records imported
    expect_equal(importRecords(rcon, 
                               data = NewRecords, 
                               returnContent = "count"), 
                 "2")
    
    # Import new records returnContent = ids returns IDs of records imported
    
    expect_equal(importRecords(rcon, 
                               data = NewRecords, 
                               returnContent = "ids"), 
                 data.frame(id = c("delete.me", "delete.too")))
    
    # Import new records returnContent = nothing returns ""
    expect_equal(importRecords(rcon, 
                               data = NewRecords, 
                               returnContent = "nothing"), 
                 "")
    
    # clean up
    deleteRecords(rcon, 
                  records = c("delete.me", "delete.too"))
  }
)

test_that(
  "importRecords returnData", 
  {
    # Return the formatted data frame ready for import
    
    Prepped <- importRecords(rcon, 
                             data = NewRecords, 
                             returnData = TRUE)
    
    expect_data_frame(Prepped)
    
    # Checked and unchecked are not 0/1
    expect_true(all(Prepped$prereq_checkbox___1 %in% c(0, 1)))
  }
)

test_that(
  "Save log to a file", 
  {
    # Save the import validation to a file
    logfile_path <- file.path(tempdir(), "logfile.txt")
    
    importRecords(rcon, 
                  data = NewRecords, 
                  logfile = logfile_path, 
                  returnData = TRUE)
    
    expect_true(file.exists(logfile_path))
    file.remove(logfile_path)
  }
)

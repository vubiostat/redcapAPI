context("Export Logging Functionality")

# API works versus "minute" so current time is rounded to minute
# Also, this needs to be in the timezone of the server, which ours is CDT
# If another user wishes to automate these test this could be an ENV variable.
systime <- as.POSIXct(Sys.time(), tz="America/Chicago")
BEGIN_TIME <- as.POSIXct(
  format(systime, format = "%Y-%m-%d %H:%M"), tz="America/Chicago") - 7*24*60*60
RecentLog  <- exportLogging(rcon, beginTime=BEGIN_TIME)

test_that(
  "Logs can be batched and match unbatched",
  {
    endTime <- BEGIN_TIME+6*24*60*60-27*60 # End time is 27min less than 6 days to test final boundary
    BatchedLog <- exportLogging(rcon, beginTime=BEGIN_TIME, endTime=endTime, batchInterval=1)
    SameLog <- RecentLog[RecentLog$timestamp >= BEGIN_TIME & RecentLog$timestamp < endTime,]
    
    skip_if(nrow(BatchedLog) == 0, "No logs for test window in past")
    
    expect_equal(BatchedLog$timestamp, SameLog$timestamp)
    expect_equal(BatchedLog$action,    SameLog$action)
    expect_equal(BatchedLog$username,  SameLog$username)
    expect_equal(BatchedLog$details,   SameLog$details)
    expect_equal(as.character(BatchedLog$record), as.character(SameLog$record))
  }
)

test_that(
  "Logs are returned for the last 7 days", 
  {
    checkmate::expect_data_frame(RecentLog)
  }
)

test_that(
  "Logs are returned for logtype = 'export'",
  {
    Logs <- exportLogging(rcon, 
                          logtype = "export", 
                          beginTime = BEGIN_TIME)
    
    all_export_record <- all(grepl("export", Logs$action, ignore.case = TRUE))
    expect_true(all_export_record)
  }
)


test_that(
  "Logs are returned for logtype = 'manage'",
  {
    Logs <- exportLogging(rcon, 
                          logtype = "manage", 
                          beginTime = BEGIN_TIME)
    
    all_manage_record <- all(grepl("Manage", Logs$action))
    expect_true(all_manage_record)
  }
)


test_that(
  "Logs are returned for logtype = 'user'",
  {
    Logs <- exportLogging(rcon, 
                          logtype = "user", 
                          beginTime = BEGIN_TIME)
    # API token creations are classified under "Manage"
    all_user_record <- all(grepl("(user|Manage| )", Logs$action, ignore.case = TRUE))
    expect_true(all_user_record)
  }
)


test_that(
  "Logs are returned for logtype = 'record'",
  {
    Logs <- exportLogging(rcon, 
                          logtype = "record", 
                          beginTime = BEGIN_TIME)
    all_record_record <- all(grepl("(Manage|(R|r)ecord)", Logs$action))
    expect_true(all_record_record)
  }
)


test_that(
  "Logs are returned for logtype = 'record_add'",
  {
    Logs <- exportLogging(rcon, 
                          logtype = "record_add", 
                          beginTime = BEGIN_TIME)
    all_add_record <- all(grepl("Create record", Logs$action))
    expect_true(all_add_record)
  }
)


test_that(
  "Logs are returned for logtype = 'record_edit'",
  {
    Logs <- exportLogging(rcon, 
                          logtype = "record_edit", 
                          beginTime = BEGIN_TIME)
    all_edit_record <- all(grepl("Update record", Logs$action))
    expect_true(all_edit_record)
  }
)


test_that(
  "Logs are returned for logtype = 'record_delete", 
  {
    Logs <- exportLogging(rcon, 
                          logtype = "record_delete", 
                          beginTime = BEGIN_TIME)
    all_delete_record <- all(grepl("Delete record", Logs$action))
    expect_true(all_delete_record)
  }
)


test_that(
  "Logs are returned for logtype = 'lock_record'", 
  {
    Logs <- exportLogging(rcon = rcon, 
                          logtype = "lock_record", 
                          beginTime = BEGIN_TIME)
    all_lock_record <- all(grepl("Lock[/]Unlock", Logs$action))
    expect_true(all_lock_record)
  }
)


test_that(
  "Logs are returned for logtype = 'page_view'", 
  {
    Logs <- exportLogging(rcon = rcon, 
                          logtype = "page_view", 
                          beginTime = BEGIN_TIME)
    all_page_view_record <- all(grepl("Page View", Logs$action))
    expect_true(all_page_view_record)
  }
)


test_that(
  "Logs are returned for an existing user", 
  {
    user_in_log <- unique(RecentLog$username)
    user_for_test <- sample(user_in_log, 1)
    skip_if(length(user_for_test) == 0)
    
    Logs <- exportLogging(rcon, 
                          user = user_for_test, 
                          beginTime = BEGIN_TIME)
    all_user <- all(Logs$user == user_for_test)
    expect_true(all_user)
  }
)


test_that(
  "Empty logs are returned for a non-existing user", 
  {
    Logs <- exportLogging(rcon, 
                          user = "this user doesn't exist", 
                          beginTime = BEGIN_TIME)
    expect_true(nrow(Logs) == 0)
  }
)



test_that(
  "Logs are returned for an existing record", 
  {
    Records <- RecentLog[!is.na(RecentLog$record) & !grepl("user", RecentLog$action), ]
    records <- RecentLog$record
    records <- trimws(records)
    records <- unique(records)
    
    record_for_test <- sample(records, 1)
    
    Logs <- exportLogging(rcon, 
                          record = record_for_test, 
                          beginTime = BEGIN_TIME)
    all_record <- all(Logs$record %in% c(record_for_test, NA))
    expect_true(all_record)
  }
)


test_that(
  "Empty logs are returned for a non-existing record", 
  {
    record_for_test <- "thisisprobablynotarecordid"
    
    Logs <- exportLogging(rcon, 
                          record = record_for_test, 
                          beginTime = BEGIN_TIME)
    
    expect_true(nrow(Logs) == 0)
  }
)


test_that(
  "Logs are returned for an existing Data Access Group", 
  {
    dag_for_test <- "temporary_dag"
    Logs <- exportLogging(rcon, 
                          dag = dag_for_test, 
                          beginTime = BEGIN_TIME)
    
    expect_true(nrow(Logs) > 1)
  }
)


test_that(
  "Logs are returned after a beginTime", 
  {
    times <- RecentLog$timestamp
    times <- sort(times)
    index <- seq_along(times)
    index <- median(index)
    
    time_this_test <- times[index]
    
    Logs <- exportRecords(rcon, 
                          beginTime = time_this_test, 
                          beginTime = BEGIN_TIME)
    
    all_after_begin <- all(Logs$timestamp >= time_this_test)
    expect_true(all_after_begin)
  }
)

test_that(
  "Logs are returned before an endTime", 
  {
    times <- RecentLog$timestamp
    times <- sort(times)
    index <- seq_along(times)
    index <- median(index)
    
    time_this_test <- times[index]
    
    Logs <- exportRecords(rcon, 
                          endTime = time_this_test, 
                          beginTime = BEGIN_TIME)
    
    all_before_end <- all(Logs$timestamp <= time_this_test)
    expect_true(all_before_end)
  }
)

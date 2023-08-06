context("Export Typed Records Offline Argument Validation")

load(file.path(test_path("testdata"), "RedcapProject_test_redcapAPI.Rdata"))

suppressWarnings({
  roff <- offlineConnection(meta_data = RedcapProject_test_redcapAPI$meta_data, 
                            arms = RedcapProject_test_redcapAPI$arms, 
                            events = RedcapProject_test_redcapAPI$events, 
                            mapping = RedcapProject_test_redcapAPI$mappings, 
                            project_info = RedcapProject_test_redcapAPI$project_information, 
                            repeat_instrument = RedcapProject_test_redcapAPI$repeating_instruments, 
                            records = RedcapProject_test_redcapAPI$records)
})

test_that(
  "Return an error if rcon is not a redcapConnection",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped.redcapOfflineConnection(mtcars), 
                 "'rcon': Must inherit from class 'redcapOfflineConnection'")
  }
)

test_that(
  "Return an error if fields is not character or otherwise invalid",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(roff, 
                                    fields = 1), 
                 "'fields': Must be of type 'character'")
    
    expect_error(exportRecordsTyped(roff, 
                                    fields = "not a field"), 
                 "'fields': Must be a subset of")
  }
)

test_that(
  "Return an error if drop_fields is not a character", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(exportRecordsTyped(roff, 
                                    drop_fields = 1), 
                 "'drop_fields': Must be of type 'character'")
    
    expect_error(exportRecordsTyped(roff, 
                                    drop_fields = "not a field"), 
                 "'drop_fields': Must be a subset of")
  }
)

test_that(
  "Return an error if forms is not a character", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(exportRecordsTyped(roff, 
                                    forms = 1), 
                 "'forms': Must be of type 'character'")
    
    expect_error(exportRecordsTyped(roff, 
                                    forms = "not a field"), 
                 "'forms': Must be a subset of")
  }
)

test_that(
  "Return an error if records is not character or numeric", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(exportRecordsTyped(roff, 
                                    records = list(123)), 
                 "'records': Must be of type 'character'")
  }
)

test_that(
  "Return an error if events is not a character", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(exportRecordsTyped(roff, 
                                    events = 1), 
                 "'events': Must be of type 'character'")
  }
)


test_that(
  "Return an error if na is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(roff,
                                    na=1:3),
                 "'na'[:] Must be of type 'list'")
    expect_error(exportRecordsTyped(roff,
                                    na=list("a")),
                 "'na'[:] Must have names")
  }
)

test_that(
  "Return an error if validation is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(roff,
                                    validation=1:3),
                 "'validation'[:] Must be of type 'list'")
    expect_error(exportRecordsTyped(roff,
                                    validation=list("a")),
                 "'validation'[:] Must have names")
  }
)

test_that(
  "Return an error if cast is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(roff,
                                    cast=1:3),
                 "'cast'[:] Must be of type 'list'")
    expect_error(exportRecordsTyped(roff,
                                    cast=list("a")),
                 "'cast'[:] Must have names")
  }
)

test_that(
  "Return an error if assignment is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportRecordsTyped(roff,
                                    assignment=1:3),
                 "'assignment'[:] Must be of type 'list'")
    expect_error(exportRecordsTyped(roff,
                                    assignment=list("a")),
                 "'assignment'[:] Must have names")
  }
)

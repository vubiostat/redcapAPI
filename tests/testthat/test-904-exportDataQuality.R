context("exportDataQuality.R")

prefix <- 'vanderbilt_dataQuality'

test_that("Data queries can be exported",{
  skip_if(!RUN_DATAQUALITY_TEST, 
        "No Data Quality Project provided. Tests Skipped")
  
  dq <- exportDataQuality(dqrcon, prefix)
  expect_gte(length(dq), 1)
  
  expect_contains(names(dq),
    c("status_id",            "rule_id",       "pd_rule_id",
      "non_rule",             "project_id",    "record",
      "event_id",             "field_name",    "repeat_instrument",
      "instance",              "status",        "exclude",
      "query_status",         "group_id",      "assigned_username", 
      "res_id",               "ts",            "response_requested",
      "response",             "comment",       "username",
      "current_query_status", "upload_doc_id", "field_comment_edited")
  )
})

test_that(
  "Return error messages if Data Quality Module not enabled",
  {
    expect_error(exportDataQuality(rcon, prefix), 
                 "Make sure the Data Quality API module is enabled in your project.")
  }
)


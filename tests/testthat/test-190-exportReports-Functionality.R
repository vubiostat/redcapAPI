context("Export Reports Functionality")

test_that(
  "Export Report with defaults", 
  {
    skip_if(!RUN_REPORTS_TEST, 
            "EXPORT_REPORTS_ID is not provided. Testing Skipped")
    expect_silent(exportReports(rcon, 
                                report_id = EXPORT_REPORTS_ID))
  }
)

test_that(
  "Export Report with raw_or_label", 
  {
    RepRaw <- exportReports(rcon, 
                            report_id = EXPORT_REPORTS_ID, 
                            raw_or_label = "raw")
    
    expect_true(all(RepRaw$dropdown_test %in% c(NA, 1:3)))
    expect_true(all(RepRaw$radio_test %in% c(NA, "a", "b", "c")))
    expect_true(all(RepRaw$checkbox_test___x %in% c(NA, 0:1)))
    
    RepLabel <- exportReports(rcon, 
                              report_id = EXPORT_REPORTS_ID, 
                              raw_or_label = "label")
    
    expect_true(all(RepLabel$dropdown_test %in% c(NA, "Blue", "Lavender", "Green")))
    expect_true(all(RepLabel$radio_test %in% c(NA, "Carnation", "Daffodil", "Tulip")))
    expect_true(all(RepLabel$checkbox_test___x %in% c(NA, "Unchecked", "Checked")))
  }
)

test_that(
  "Export Report with raw_or_label_headers", 
  {
    RepRaw <- exportReports(rcon, 
                            report_id = EXPORT_REPORTS_ID, 
                            raw_or_label_headers = "raw")
    RepLabel <- exportReports(rcon, 
                              report_id = EXPORT_REPORTS_ID, 
                              raw_or_label_headers = "label")
    
    expect_false(any(names(RepRaw) == names(RepLabel)))
  }
)

test_that(
  "Export Report with export_checkbox_label", 
  {
    RepRawFalse <- exportReports(rcon, 
                                 report_id = EXPORT_REPORTS_ID, 
                                 raw_or_label = "raw", 
                                 export_checkbox_label = FALSE)
    expect_true(all(RepRawFalse$checkbox_test___x %in% c(NA, 0:1)))
    
    RepRawTrue <- exportReports(rcon, 
                                 report_id = EXPORT_REPORTS_ID, 
                                 raw_or_label = "raw", 
                                 export_checkbox_label = TRUE)
    expect_true(all(RepRawTrue$checkbox_test___x %in% c(NA, 0:1)))
    
    RepLabelFalse <- exportReports(rcon, 
                                 report_id = EXPORT_REPORTS_ID, 
                                 raw_or_label = "label", 
                                 export_checkbox_label = FALSE)
    expect_true(all(RepLabelFalse$checkbox_test___x %in% c(NA, "Unchecked", "Checked")))
    
    RepLabelTrue <- exportReports(rcon, 
                                 report_id = EXPORT_REPORTS_ID, 
                                 raw_or_label = "label", 
                                 export_checkbox_label = TRUE)
    expect_true(all(RepLabelTrue$checkbox_test___x %in% c(NA, "Guitar")))
  }
)

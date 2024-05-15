context("Export Typed Report Functionality")

report_ids <- EXPORT_REPORTS_ID

test_that(
  "exportReportsTyped with default settings", 
  {
    skip_if(!RUN_REPORTS_TEST, 
            "No value provided for EXPORT_REPORTS_ID. Tests Skipped")
    Report <- expect_silent(exportReportsTyped(rcon, 
                                               report_id = EXPORT_REPORTS_ID[1]))
    
    if ("dropdown_test" %in% names(Report)){
      expect_class(Report$dropdown_test, "factor")
      expect_equal(levels(Report$dropdown_test), 
                   c("Green", "Blue", "Lavender")) 
    }
    
    if ("radio_test" %in% names(Report)){
      expect_class(Report$radio_test, "factor")
      expect_equal(levels(Report$radio_test), 
                   c("Carnation", "Tulip", "Daffodil")) 
    }
    
    if ("truefalse_test" %in% names(Report)){
      expect_logical(Report$truefalse_test)
    }
    
    if ("yesno_test" %in% names(Report)){
      expect_class(Report$yesno_test, "factor")
      expect_equal(levels(Report$yesno_test),
                   c("No", "Yes"))      
    }

    if ("checkbox_test___x" %in% names(Report)){
      expect_class(Report$checkbox_test___x, "factor")
      expect_equal(levels(Report$checkbox_test___x),
                   c("Unchecked", "Checked"))
    }
    
    if ("number_1dp_comma_test" %in% names(Report)){
      expect_numeric(Report$number_1dp_comma_test)
    }
    
    if ("number_test" %in% names(Report)){
      expect_numeric(Report$number_test)
    }
  }
)

test_that(
  "exportReportsTyped responds to drop_fields", 
  {
    skip_if(!RUN_REPORTS_TEST, 
            "No value provided for EXPORT_REPORTS_ID. Tests Skipped")
    ReportOrig <- exportReportsTyped(rcon, 
                                     report_id = EXPORT_REPORTS_ID[1])
    drop_eligible <- names(ReportOrig)[!grepl("checkbox", names(ReportOrig))]
    drop <- tail(drop_eligible, 1)
    
    Report <- exportReportsTyped(rcon, 
                                 report_id = EXPORT_REPORTS_ID[1], 
                                 drop_fields = drop)
    
    expect_false(drop %in% names(Report))
  }
)

test_that(
  "exportReportsTyped with a raw cast", 
  {
    skip_if(!RUN_REPORTS_TEST, 
            "No value provided for EXPORT_REPORTS_ID. Tests Skipped")
    Report <- expect_silent(exportReportsTyped(rcon, 
                                               report_id = EXPORT_REPORTS_ID[1], 
                                               cast = raw_cast))
    
    if ("dropdown_test" %in% names(Report)){
      expect_numeric(Report$dropdown_test)
      expect_equal(sort(unique(Report$dropdown_test)), 
                   1:3)
    }
    
    if ("radio_test" %in% names(Report)){
      expect_class(Report$radio_test, "character")
      expect_equal(sort(unique(Report$radio_test)), 
                   c("a", "b", "c")) 
    }
    
    if ("truefalse_test" %in% names(Report)){
      expect_numeric(Report$truefalse_test)
      expect_equal(sort(unique(Report$truefalse_test)), 
                   0:1)
    }
    
    if ("yesno_test" %in% names(Report)){
      expect_numeric(Report$yesno_test)
      expect_equal(sort(unique(Report$yesno_test)),
                   0:1)
    }
    
    if ("checkbox_test___x" %in% names(Report)){
      expect_numeric(Report$checkbox_test___x)
      expect_equal(sort(unique(Report$checkbox_test___x)),
                   0:1)
    }
    
    if ("number_1dp_comma_test" %in% names(Report)){
      expect_numeric(Report$number_1dp_comma_test)
    }
    
    if ("number_test" %in% names(Report)){
      expect_numeric(Report$number_test)
    }
  }
)

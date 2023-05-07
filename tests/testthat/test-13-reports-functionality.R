context("exportReports functionality")


#####################################################################
# exportReports                                                  ####

test_that("reports can be exported",{
  expect_silent(rep <- exportReports(rcon, 357209))
})

test_that(
  "fields in the drop= arg are not returned", 
  {
    fields_to_drop <- c("treatment")
    Report <- exportReports(rcon, 357209,
                            drop = fields_to_drop)
  }
)

#####################################################################
# exportReportsTyped                                             ####

test_that(
  "exportReportsTyped with default settings", 
  {
    Report <- expect_silent(exportReportsTyped(rcon, 
                                               report_id = 362756))
    
    expect_class(Report$dropdown_test, "factor")
    expect_equal(levels(Report$dropdown_test), 
                 c("Green", "Blue", "Lavender"))
    
    expect_logical(Report$truefalse_test)
    
    expect_class(Report$yesno_test, "factor")
    expect_equal(levels(Report$yesno_test),
                 c("No", "Yes"))
    
    expect_class(Report$yesno_test, "factor")
    expect_equal(levels(Report$checkbox_test___x),
                 c("Unchecked", "Checked"))
    
    expect_numeric(Report$number_1dp_comma_test)
    expect_numeric(Report$number_test)
  }
)

test_that(
  "exportReportsTyped responds to drop_fields", 
  {
    Report <- expect_silent(exportReportsTyped(rcon, 
                                               report_id = 362756, 
                                               drop_fields = c("number_test", 
                                                               "radio_test", 
                                                               "checkbox_test___x")))
    
    expect_true(!any(c("number_test", "radio_test", "checkbox_test___x") %in% 
                       names(Report)))
  }
)

test_that(
  "exportReportsTyped with a raw cast", 
  {
    Report <- expect_silent(exportReportsTyped(rcon, 
                                               report_id = 362756, 
                                               cast = raw_cast))
    
    expect_numeric(Report$dropdown_test)
    expect_equal(sort(unique(Report$dropdown_test)), 
                 1:3)
    
    expect_numeric(Report$truefalse_test)
    expect_equal(sort(unique(Report$truefalse_test)), 
                 0:1)
    
    expect_numeric(Report$yesno_test)
    expect_equal(sort(unique(Report$yesno_test)),
                 0:1)
    
    expect_numeric(Report$checkbox_test___x)
    expect_equal(sort(unique(Report$checkbox_test___x)),
                 0:1)
    
    expect_numeric(Report$number_1dp_comma_test)
    expect_numeric(Report$number_test)
  }
)

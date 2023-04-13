context("recastData.R")

rcon <- redcapConnection(url = url, 
                         token = API_KEY)

fields <- c("dropdown_test",
            "radio_test", 
            "checkbox_test", 
            "yesno_test", 
            "truefalse_test")

export_fields <- c("dropdown_test",
                   "radio_test", 
                   "checkbox_test___x",
                   "checkbox_test___y", 
                   "checkbox_test___z",
                   "yesno_test", 
                   "truefalse_test")

Records <- exportRecordsTyped(rcon, 
                              fields = fields)

#####################################################################
# Functionality                                                  ####

test_that(
  "Successful recast with defaults", 
  {
    Recast <- recastData(Records, 
                         rcon = rcon, 
                         fields = names(Records))
    
    expect_equal(levels(Recast$dropdown_test), c("1", "2", "3"))
    expect_equal(levels(Recast$radio_test), c("a", "b", "c"))
    expect_equal(sort(unique(Recast$yesno_test)), c(0, 1))
    expect_equal(sort(unique(Recast$truefalse_test)), c(0, 1))
    expect_equal(unique(Recast$checkbox_test___x), c(0, 1))
    expect_equal(unique(Recast$checkbox_test___y), c(0, 1))
    expect_equal(unique(Recast$checkbox_test___z), c(0, 1))
  }
)

test_that(
  "Handle numeric indexing appropriately", 
  {
    local_reproducible_output(width = 200)
    Recast <- recastData(Records, 
                         rcon = rcon, 
                         fields = 1:3)
    
    expect_equal(levels(Recast$dropdown_test), c("1", "2", "3"))
    expect_equal(levels(Recast$radio_test), c("a", "b", "c"))
    expect_equal(unique(Recast$checkbox_test___x), c(0, 1))
    expect_equal(levels(Recast$checkbox_test___y), levels(Records$checkbox_test___y))
    expect_equal(levels(Recast$checkbox_test___z), levels(Records$checkbox_test___z))
    expect_equal(levels(Recast$yesno_test), c("No", "Yes"))
    expect_true(is.logical(Recast$truefalse_test))
    
    Recast <- recastData(Records, 
                         rcon = rcon, 
                         fields = c(2, 4, 6))
    
    expect_equal(levels(Recast$dropdown_test), levels(Records$dropdown_test))
    expect_equal(levels(Recast$radio_test), c("a", "b", "c"))
    expect_equal(levels(Recast$checkbox_test___x), levels(Records$checkbox_test___x))
    expect_equal(sort(unique(Recast$checkbox_test___y)), 0:1)
    expect_equal(levels(Recast$checkbox_test___z), levels(Records$checkbox_test___z))
    expect_equal(sort(unique(Recast$yesno_test)), 0:1)
    expect_true(is.logical(Recast$truefalse_test))
    
    expect_error(recastData(Records, 
                           rcon = rcon, 
                           fields = c(6:12)), 
                "Columns [{]8, 9, 10, 11, 12[}] requested in a data frame with 7 columns")
  }
)

test_that(
  "Handle logical indexing appropriately", 
  {
    local_reproducible_output(width = 200)
    Recast <- recastData(Records, 
                         rcon = rcon, 
                         fields = rep(c(TRUE, FALSE), c(3, 4)))
    
    expect_equal(levels(Recast$dropdown_test), c("1", "2", "3"))
    expect_equal(levels(Recast$radio_test), c("a", "b", "c"))
    expect_equal(unique(Recast$checkbox_test___x), c(0, 1))
    expect_equal(levels(Recast$checkbox_test___y), levels(Records$checkbox_test___y))
    expect_equal(levels(Recast$checkbox_test___z), levels(Records$checkbox_test___z))
    expect_equal(levels(Recast$yesno_test), c("No", "Yes"))
    expect_true(is.logical(Recast$truefalse_test))
    
    Recast <- recastData(Records, 
                         rcon = rcon, 
                         fields = rep(c(FALSE, TRUE), 
                                      length.out = ncol(Records)))
    
    expect_equal(levels(Recast$dropdown_test), levels(Records$dropdown_test))
    expect_equal(levels(Recast$radio_test), c("a", "b", "c"))
    expect_equal(levels(Recast$checkbox_test___x), levels(Records$checkbox_test___x))
    expect_equal(sort(unique(Recast$checkbox_test___y)), 0:1)
    expect_equal(levels(Recast$checkbox_test___z), levels(Records$checkbox_test___z))
    expect_equal(sort(unique(Recast$yesno_test)), 0:1)
    expect_true(is.logical(Recast$truefalse_test))
    
    expect_error(recastData(Records, 
                            rcon = rcon, 
                            fields = rep(c(FALSE, TRUE), 
                                         length.out = 3)), 
                 "'fields' [(]logical[)] should be of length 7 and is length 3")
  }
)

test_that(
  "Suffixes applied correctly", 
  {
    Recast <- recastData(Records, 
                         rcon = rcon, 
                         fields = names(Records), 
                         suffix = "_coded")
    
    expect_equal(levels(Recast$dropdown_test),     levels(Records$dropdown_test))
    expect_equal(levels(Recast$radio_test),        levels(Recast$radio_test))
    expect_equal(levels(Recast$checkbox_test___x), levels(Records$checkbox_test___x))
    expect_equal(levels(Recast$checkbox_test___y), levels(Records$checkbox_test___x))
    expect_equal(levels(Recast$checkbox_test___z), levels(Records$checkbox_test___z))
    expect_equal(levels(Recast$yesno_test),        levels(Records$yesno_test))
    expect_true(is.logical(Recast$truefalse_test))
    
    expect_equal(levels(Recast$dropdown_test_coded),           c("1", "2", "3"))
    expect_equal(levels(Recast$radio_test_coded),              c("a", "b", "c"))
    expect_equal(sort(unique(Recast$checkbox_test___x_coded)), 0:1)
    expect_equal(sort(unique(Recast$checkbox_test___y_coded)), 0:1)
    expect_equal(sort(unique(Recast$checkbox_test___z_coded)), 0:1)
    expect_equal(sort(unique(Recast$yesno_test_coded)),        0:1)
    expect_equal(sort(unique(Recast$truefalse_test_coded)),    0:1)
  }
)

test_that(
  "Handle mChoice fields by skipping them", 
  {
    hmisc_installed <- require(Hmisc)
    
    skip_if(!hmisc_installed, 
            "Hmisc is not installed")
    
    Records <- exportRecordsTyped(rcon, 
                                  fields = fields, 
                                  mChoice = TRUE)
    
    Recast <- recastData(Records, 
                         rcon = rcon, 
                         fields = names(Records))
    
    expect_class(Recast$checkbox_test, "mChoice")
    expect_identical(Records$checkbox_test, 
                     Recast$checkbox_test)
    
    detach("package:Hmisc", unload = TRUE)
  }
)

#####################################################################
# Argument Validation                                            ####

test_that(
  "Return an error if data is not a data.frame", 
  {
    local_reproducible_output(width = 200)
    expect_error(recastData(data = "not data", 
                            rcon = rcon, 
                            fields = export_fields), 
                 "Variable 'data': Must be of type 'data.frame'")
  }
)

test_that(
  "Return an error if rcon is not a redcapConnection object", 
  {
    local_reproducible_output(width = 200)
    expect_error(recastData(data = Records, 
                            rcon = "not a thing", 
                            fields = export_fields), 
                 "Variable 'rcon': Must inherit from class 'redcapConnection'")
  }
)

test_that(
  "Return an error if fields is not numeric, character, or logical", 
  {
    local_reproducible_output(width = 200)
    expect_error(recastData(data = Records, 
                            rcon = rcon, 
                            fields = mtcars), 
                 "'fields': One of the following must apply")
  }
)

test_that(
  "Return an error if recast is not a named list", 
  {
    local_reproducible_output(width = 200)
    expect_error(recastData(data = Records, 
                            rcon = rcon, 
                            fields = export_fields, 
                            cast = "this isn't a list"), 
                 "Variable 'cast': Must be of type 'list'")
    
    expect_error(recastData(data = Records, 
                            rcon = rcon, 
                            fields = export_fields, 
                            cast = list(uncastChecked)), 
                 "Variable 'cast': Must have names.")
  }
)

test_that(
  "Return an error if suffix is not character(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(recastData(data = Records, 
                            rcon = rcon, 
                            fields = export_fields, 
                            suffix = 123), 
                 "Variable 'suffix': Must be of type 'character'")
    
    expect_error(recastData(data = Records, 
                            rcon = rcon, 
                            fields = export_fields, 
                            suffix = c("_label", ".label")), 
                 "Variable 'suffix': Must have length 1")
  }
)

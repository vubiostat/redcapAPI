context("recastData.R")

rcon <- redcapConnection(url = url, 
                         token = API_KEY)

#####################################################################
# Functionality

#####################################################################
# Argument Validation

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

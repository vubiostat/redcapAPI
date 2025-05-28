context("Missing Summary")

DesiredOutput <- 
  structure(
    list(
      record_id = structure(as.character(1:20), 
                            label = "Record ID"),
      redcap_event_name = rep("event_1_arm_1", 20),
      redcap_data_access_group = rep(NA_character_, 20),
      redcap_repeat_instrument = rep(NA_character_, 20),
      redcap_repeat_instance = rep(NA_character_, 20),
      n_missing = c(6, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 5), 
      missing = c("row_purpose, prereq_radio, prereq_number, prereq_date, prereq_yesno, no_prereq_number", 
                  "", 
                  "", 
                  "", 
                  "one_prereq_non_checkbox", 
                  "", 
                  "one_prereq_checkbox", 
                  "", 
                  "two_prereq_and", 
                  "", 
                  "two_prereq_or", 
                  "", 
                  "two_prereq_and_one_check", 
                  "two_prereq_and_one_check", 
                  "", 
                  "three_prereq_andor", 
                  "", 
                  "one_prereq_inequality", 
                  "", 
                  "prereq_radio, prereq_number, prereq_date, prereq_yesno, no_prereq_number")
    ), 
    row.names = c(NA, -20L), 
    class = "data.frame"
  )

importRecords(rcon, 
              data.frame(record_id = 1:20, 
                         redcap_data_access_group = rep(NA_character_, 20)), 
              overwriteBehavior = "overwrite")

# Test functionality ------------------------------------------------

test_that(
  "Missing values are correctly identified around branching logic",
  {
    local_reproducible_output(width = 200)
    expect_identical(
      missingSummary(rcon,
                     dag = TRUE,
                     fields = "record_id", 
                     records = as.character(1:20), 
                     forms = "branching_logic"), 
      DesiredOutput
    )
  }
)

# Test for argument validation --------------------------------------

test_that(
  "Return an error if `rcon` is not a redcapConnection object",
  {
    local_reproducible_output(width = 200)
    expect_error(
      missingSummary(mtcars),
      "no applicable method for 'missingSummary'"
    )
  }
)

test_that(
  "Return an error if `excludeMissingForms` is not logical", 
  {
    local_reproducible_output(width = 200)
    expect_error(
      missingSummary(rcon, 
                     excludeMissingForms = "TRUE"), 
      "Variable 'excludeMissingForms': Must be of type 'logical'"
    )
  }
)

test_that(
  "Return an error if `excludeMissingForms` is not length 1", 
  {
    local_reproducible_output(width = 200)
    expect_error(
      missingSummary(rcon, 
                     excludeMissingForms = c(TRUE, FALSE)), 
      "Variable 'excludeMissingForms': Must have length 1, but has length 2."
    )
  }
)


test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)
   
    expect_error(missingSummary(rcon, 
                                config = list(1)), 
                 "'config': Must have names")
    expect_error(missingSummary(rcon, 
                                config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(missingSummary(rcon, 
                                api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(missingSummary(rcon, 
                                api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
    
    expect_silent(missingSummary(rcon,
                                 exportRecordsArgs = list(fields = "record_id", 
                                                          records = as.character(1:20), 
                                                          forms = "branching_logic")))
  }
)

# Test internal function for checkbox -------------------------------

md <- data.frame(
  field_name = c('record_id','cb1','cb2','cb3'),
  form_name = 'test',
  field_type = c('text','checkbox','checkbox','checkbox'),
  select_choices_or_calculations = c(NA, '1, yes | 2, no', '1, yes | 2, no', '1, yes | 2, no')
)
logic <- list(record_id = NA, cb1 = NA, cb2 = expression(TRUE), cb3 = expression(TRUE))
r <- data.frame(record_id = 1:2, cb1___1 = 0, cb1___2 = 0:1, cb2___1 = 0, cb2___2 = 0:1, cb3___1 = 0, cb3___2 = 0:1, test_complete = 2)
o <- data.frame(record_id = 1:2, cb1___1 = FALSE, cb1___2 = FALSE, cb2 = c(TRUE, FALSE), cb3 = c(TRUE, FALSE), test_complete = 2)

test_that(
  "Missing values in checkbox groups are correctly identified around branching logic",
  {
    local_reproducible_output(width = 200)
    expect_identical(
	  redcapAPI:::.missingSummary_isMissingInField(r, md, logic), o
    )
  }
)

md <- data.frame(
  field_name = c('record_id','badfield'),
  form_name = 'test',
  field_type = c('text','text'),
  select_choices_or_calculations = c(NA, NA)
)
logic <- list(record_id = NA, badfield = expression(stop('logic fail!')))
r <- data.frame(record_id = 1:2, badfield = 'stop', test_complete = 2)
o <- data.frame(record_id = 1:2, badfield = FALSE, test_complete = 2)

test_that(
  "Missing value checks should handle errors in branching logic",
  {
    local_reproducible_output(width = 200)
    expect_identical(
	  redcapAPI:::.missingSummary_isMissingInField(r, md, logic), o
    )
  }
)

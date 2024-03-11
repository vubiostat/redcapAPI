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

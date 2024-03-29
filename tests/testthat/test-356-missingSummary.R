context("Missing Summary")

DesiredOutput <- 
  structure(
    list(
      record_id = structure(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
                              "11", "12", "13", "14", "15", "16", "17", "18", "19", "20"), 
                            label = "Record ID", 
                            class = c("labelled", "character")),
      redcap_event_name = c("event_1_arm_1", "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", 
                            "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", 
                            "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", 
                            "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", 
                            "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", "event_1_arm_1"),
      redcap_data_access_group = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
      redcap_repeat_instrument = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
      redcap_repeat_instance = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
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
                     exportRecordsArgs = list(fields = "record_id", 
                                              records = as.character(1:20), 
                                              forms = "branching_logic")), 
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
  "Return an error if exportRecordsArgs is not a list.",
  # * return an error if exportRecordsArgs is not a named list.
  # * return an error if exportRecordsArgs has elements that are not arguments to exportRecords.
  {
    local_reproducible_output(width = 200)
    expect_error(
      missingSummary(rcon, 
                     exportRecordsArgs = "branching_logic"), 
      "'exportRecordsArgs': Must be of type 'list'"
    )
  }
)

test_that(
  "Return an error if exportRecordsArgs is not a named list.",
  {
    local_reproducible_output(width = 200)
    expect_error(
      missingSummary(rcon, 
                     exportRecordsArgs = list("branching_logic")), 
      "'exportRecordsArgs': Must have names"
    )
  }
)

test_that(
  "Return an error if `fixed_fields` is not a character vector",
  {
    local_reproducible_output(width = 200)
    expect_error(
      missingSummary(rcon, 
                     fixed_fields = 1:3), 
      "Variable 'fixed_fields': Must be of type 'character'"
    )
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(missingSummary(rcon, 
                                error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
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
  }
)

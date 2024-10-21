context("Rename Records Argument Validation")

test_that(
  "Return an error if rcon is not a redcapConnection",
  {
    expect_error(renameRecord(rcon = "not an rcon", 
                              record_name = "1", 
                              new_record_name = "100"), 
                 "no applicable method for 'renameRecord'")
  }
)

test_that(
  "Return an error if record_name is not character", 
  {
    expect_error(renameRecord(rcon, 
                              record_name = FALSE, 
                              new_record_name = "100"), 
                 "Variable 'record_name': Must be of type 'character'")
  }
)

test_that(
  "Return an error if new_record_name is not character(1)", 
  {
    expect_error(renameRecord(rcon, 
                              record_name = "1", 
                              new_record_name = FALSE), 
                 "Variable 'new_record_name': Must be of type 'character'")
  }
)

test_that(
  "Return an error if arm is not character(1)", 
  {
    expect_error(renameRecord(rcon, 
                              record_name = "1", 
                              new_record_name = "100", 
                              arm = TRUE), 
                 "Variable 'arm': Must be of type 'character'")
  }
)

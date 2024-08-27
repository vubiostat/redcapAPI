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
  "Return an error if record_name is not character(1)", 
  {
    expect_error(renameRecord(rcon, 
                              record_name = c("1", "2"), 
                              new_record_name = "100"), 
                 "'record_name': Must have length 1")
    
    expect_error(renameRecord(rcon, 
                              record_name = 1:2, 
                              new_record_name = "100"), 
                 "'record_name': Must have length 1")
    
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
                              new_record_name = c("100", "101")), 
                 "'new_record_name': Must have length 1")
    
    expect_error(renameRecord(rcon, 
                              record_name = 1, 
                              new_record_name = c("100", "101")), 
                 "'new_record_name': Must have length 1")
    
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
                              arm = c("1", "2")), 
                 "Variable 'arm': Must have length 1")
    
    expect_error(renameRecord(rcon, 
                              record_name = "1", 
                              new_record_name = "100", 
                              arm = TRUE), 
                 "Variable 'arm': Must be of type 'character'")
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)
   
    expect_error(renameRecord(rcon, 
                              record_name = "1", 
                              new_record_name = "100", 
                              config = list(1)), 
                 "'config': Must have names")
    expect_error(renameRecord(rcon, 
                              record_name = "1", 
                              new_record_name = "100",
                              config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(renameRecord(rcon, 
                              record_name = "1", 
                              new_record_name = "100", 
                              api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(renameRecord(rcon, 
                              record_name = "1", 
                              new_record_name = "100",
                              api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

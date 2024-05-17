context("Arms Methods Argument Validtion")

#####################################################################
# Export Arms Validation

test_that(
  "Return an error when rcon is not redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportArms("not an rcon"), 
                 "no applicable method for 'exportArms'")
  }
)

test_that(
  "Return an error when exportArms is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportArms(rcon, 
                            arms = c(TRUE, FALSE)), 
                 "'arms': Must be of type 'character'")
  }
)

#####################################################################
# Import Arms Validation


Arms <- data.frame(arm_num = c(1:2), 
                   name = c("Arm 1", "Arm 2"))

test_that(
  "Return an error when rcon is not redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(importArms("not an rcon", 
                            data = Arms), 
                 "no applicable method for 'importArms'")
  }
)

test_that(
  "Return an error when data is not a data.frame", 
  {
    local_reproducible_output(width = 200)
    
    ArmsImproper <- data.frame(arm_number = c("arm1", "arm2"), 
                               arm_name = 1:2)
    
    expect_error(importArms(rcon, 
                            "not a data frame"), 
                 "Variable 'data': Must be of type 'data.frame'")
    
    # Has the correct column names
    expect_error(importArms(rcon, 
                            data = ArmsImproper), 
                 "Variable 'names[(]data[)]': Must be a subset of [{]'arm_num','name'[}]")
    
    # The vectors have the correct names now, but we've given them the wrong class
    names(ArmsImproper) <- c("arm_num", "name")
    expect_error(importArms(rcon, 
                            data = ArmsImproper), 
                 "'data[$]arm_num': Must be of type 'integerish'")
    expect_error(importArms(rcon, 
                            data = ArmsImproper), 
                 "'data[$]name': Must be of type 'character'")
  }
)

test_that(
  "Return an error if override is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importArms(rcon, 
                            Arms, 
                            override = c(TRUE, FALSE)), 
                 "'override': Must have length 1")
    
    expect_error(importArms(rcon, 
                            Arms, 
                            override = "TRUE"), 
                 "'override': Must be of type 'logical'")
  }
)

test_that(
  "Validations of config and api_param", 
  {
    local_reproducible_output(width = 200)

    expect_error(importArms(rcon,
                            data = Arms,
                            config = list(1)),
                 "'config': Must have names")
    expect_error(importArms(rcon,
                            data = Arms,
                            config = "not a list"),
                 "'config': Must be of type 'list'")
    
    expect_error(importArms(rcon,
                            data = Arms,
                            api_param = list(1)),
                 "'api_param': Must have names")
    expect_error(importArms(rcon,
                            data = Arms,
                            api_param = "not a list"),
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# Delete Arms Validation

test_that(
  "Return an error when rcon is not redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(deleteArms("not an rcon"), 
                 "no applicable method for 'deleteArms'")
  }
)

test_that(
  "Return an error when deleteArms is not character", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(deleteArms(rcon, 
                            arms = c(TRUE, FALSE)), 
                 "'arms': Must be of type 'character'")
  }
)

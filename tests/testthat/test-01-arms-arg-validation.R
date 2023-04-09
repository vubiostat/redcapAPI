context("export/import/delete Arms Argument Validation")

rcon <- redcapConnection(url = url, 
                         token = API_KEY)

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

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportArms(rcon, 
                            error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(exportArms(rcon, 
                            config = list(1)), 
                 "'config': Must have names")
    expect_error(exportArms(rcon, 
                            config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportArms(rcon, 
                            api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportArms(rcon, 
                            api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
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
                            arms_data = Arms), 
                 "no applicable method for 'importArms'")
  }
)

test_that(
  "Return an error when arms_data is not a data.frame", 
  {
    local_reproducible_output(width = 200)
    
    ArmsImproper <- data.frame(arm_number = c("arm1", "arm2"), 
                               arm_name = 1:2)
    
    expect_error(importArms(rcon, 
                            "not a data frame"), 
                 "Variable 'arms_data': Must be of type 'data.frame'")
    
    # Has the correct column names
    expect_error(importArms(rcon, 
                            arms_data = ArmsImproper), 
                 "Variable 'names[(]arms_data[)]': Must be a subset of [{]'arm_num','name'[}]")
    
    # The vectors have the correct names now, but we've given them the wrong class
    names(ArmsImproper) <- c("arm_num", "name")
    expect_error(importArms(rcon, 
                            arms_data = ArmsImproper), 
                 "'arms_data[$]arm_num': Must be of type 'integerish'")
    expect_error(importArms(rcon, 
                            arms_data = ArmsImproper), 
                 "'arms_data[$]name': Must be of type 'character'")
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
  "Return an error if refresh is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importArms(rcon, 
                            Arms, 
                            refresh = c(TRUE, FALSE)), 
                 "'refresh': Must have length 1")
    
    expect_error(importArms(rcon, 
                            Arms, 
                            refresh = "TRUE"), 
                 "'refresh': Must be of type 'logical'")
  }
)

test_that(
  "Return an error when error handling isn't one of null, error", 
  {
    local_reproducible_output(width = 200)
    expect_error(importArms(rcon, 
                            arms_data = Arms,
                            error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(importArms(rcon,
                            arms_data = Arms,
                            config = list(1)),
                 "'config': Must have names")
    expect_error(importArms(rcon,
                            arms_data = Arms,
                            config = "not a list"),
                 "'config': Must be of type 'list'")

    expect_error(importArms(rcon,
                            arms_data = Arms,
                            api_param = list(1)),
                 "'api_param': Must have names")
    expect_error(importArms(rcon,
                            arms_data = Arms,
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
    
    skip_if(!STRUCTURAL_TEST_READY, 
            "Infrastructure not quite ready for structural tests.")
    
    expect_error(deleteArms("not an rcon"), 
                 "no applicable method for 'deleteArms'")
  }
)

test_that(
  "Return an error when deleteArms is not character", 
  {
    local_reproducible_output(width = 200)
    skip_if(!STRUCTURAL_TEST_READY, 
            "Infrastructure not quite ready for structural tests.")
    
    expect_error(deleteArms(rcon, 
                            arms = c(TRUE, FALSE)), 
                 "'arms': Must be of type 'character'")
  }
)

test_that(
  "Return an error if refresh is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    skip_if(!STRUCTURAL_TEST_READY, 
            "Infrastructure not quite ready for structural tests.")
    
    expect_error(deleteArms(rcon, 
                            arms = 1, 
                            refresh = c(TRUE, FALSE)), 
                 "'refresh': Must have length 1")
    
    expect_error(deleteArms(rcon, 
                            arms = 1, 
                            refresh = "TRUE"), 
                 "'refresh': Must be of type 'logical'")
  }
)

test_that(
  "Validate error_handling, config, api_param", 
  {
    local_reproducible_output(width = 200)
    skip_if(!STRUCTURAL_TEST_READY, 
            "Infrastructure not quite ready for structural tests.")
    
    expect_error(deleteArms(rcon,
                            arms = 1,
                            error_handling = "not an option"), 
                 "'error[_]handling': Must be element of set [{]'null','error'[}]")
    
    expect_error(deleteArms(rcon,
                            arms = 1,
                            config = list(1)), 
                 "'config': Must have names")
    expect_error(deleteArms(rcon,
                            arms = 1,
                            config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(deleteArms(rcon, 
                            arms = 1,
                            api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(deleteArms(rcon, 
                            arms = 1,
                            api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

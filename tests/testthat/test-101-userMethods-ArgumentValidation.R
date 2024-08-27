context("User Methods Argument Validation")

ProjectUser <- rcon$users()

if(!EXPENDABLE_USER %in% ProjectUser)
{
  importUsers(rcon, data = data.frame(username = EXPENDABLE_USER, 
                                      data_access_groups = 0))
  ProjectUser <- rcon$users()
}

#####################################################################
# Export Users                                                   ####

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportUsers("not an rcon"), 
                 "no applicable method for 'exportUsers'")
  }
)

test_that(
  "Return an error if dates is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportUsers(rcon, 
                             dates = c(TRUE, FALSE)), 
                 "'dates'[:] Must have length 1")
    expect_error(exportUsers(rcon, 
                             dates = "TRUE"), 
                 "'dates'[:] Must be of type 'logical'")
  }
)


test_that(
  "Return an error if labels is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportUsers(rcon, 
                             labels = c(TRUE, FALSE)), 
                 "'labels'[:] Must have length 1")
    expect_error(exportUsers(rcon, 
                             labels = "TRUE"), 
                 "'labels'[:] Must be of type 'logical'")
  }
)

test_that(
  "Return an error if form_rights is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportUsers(rcon, 
                             form_rights = c(TRUE, FALSE)), 
                 "'form_rights'[:] Must have length 1")
    expect_error(exportUsers(rcon, 
                             form_rights = "TRUE"), 
                 "'form_rights'[:] Must be of type 'logical'")
  }
)


test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)

    expect_error(exportUsers(rcon, 
                             config = list(1)), 
                 "'config': Must have names")
    expect_error(exportUsers(rcon, 
                             config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportUsers(rcon, 
                             api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportUsers(rcon, 
                             api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# Import Users                                                   ####

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(importUsers("not an rcon"), 
                 "no applicable method for 'importUsers'")
  }
)

test_that(
  "Return an error if data is not at data frame", 
  {
    local_reproducible_output(width = 200)
    expect_error(importUsers(rcon, 
                             data = "not data"), 
                 "'data': Must be of type 'data.frame'")
  }
)

test_that(
  "Return an error if data does not match User data structure", 
  {
    local_reproducible_output(width = 200)
    expect_error(importUsers(rcon, 
                             data = mtcars), 
                 "'names[(]data[)]': Must be a subset of")
  }
)

test_that(
  "Return an error if conslidate is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(importUsers(rcon, 
                             data = ProjectUser, 
                             consolidate = "TRUE"), 
                 "'consolidate': Must be of type 'logical'")
    expect_error(importUsers(rcon, 
                             data = ProjectUser, 
                             consolidate = c(TRUE, FALSE)), 
                 "'consolidate': Must have length 1")
  }
)


test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)

    expect_error(importUsers(rcon, 
                             data = ProjectUser,
                             config = list(1)), 
                 "'config': Must have names")
    expect_error(importUsers(rcon, 
                             data = ProjectUser,
                             config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(importUsers(rcon, 
                             data = ProjectUser,
                             api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(importUsers(rcon, 
                             data = ProjectUser,
                             api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# Delete Users                                                   ####

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteUsers("not an rcon", 
                             users = EXPENDABLE_USER), 
                 "no applicable method for 'deleteUsers'")
  }
)

test_that(
  "Return an error if users is not a character", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteUsers(rcon, 
                             users = 123), 
                 "'users': Must be of type 'character'")
  }
)


test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)

    expect_error(deleteUsers(rcon, 
                             users = EXPENDABLE_USER,
                             config = list(1)), 
                 "'config': Must have names")
    expect_error(deleteUsers(rcon, 
                             users = EXPENDABLE_USER,
                             config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(deleteUsers(rcon, 
                             users = EXPENDABLE_USER,
                             api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(deleteUsers(rcon, 
                             users = EXPENDABLE_USER,
                             api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

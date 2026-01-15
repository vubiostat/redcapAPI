#####################################################################
# exportUserRoles Argument Validation                            ####

test_that(
  "Return an error if rcon is not a redcapConnection",
  {
    local_reproducible_output(width = 200)
    expect_error(exportUserRoles("not an rcon"),
                 "no applicable method for 'exportUserRoles'")
  }
)

test_that(
  "Return an error if labels is not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportUserRoles(rcon,
                                 labels = "TRUE"),
                 "'labels': Must be of type 'logical'")
    expect_error(exportUserRoles(rcon,
                                 labels = c(TRUE, FALSE)),
                 "'labels': Must have length 1")
  }
)

test_that(
  "Return an error if form_rights is not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(exportUserRoles(rcon,
                                 form_rights = "TRUE"),
                 "'form_rights': Must be of type 'logical'")
    expect_error(exportUserRoles(rcon,
                                 form_rights = c(TRUE, FALSE)),
                 "'form_rights': Must have length 1")
  }
)

test_that(
  "Validate config, api_param",
  {
    local_reproducible_output(width = 200)

    expect_error(exportUserRoles(rcon,
                                 config = list(1)),
                 "'config': Must have names")
    expect_error(exportUserRoles(rcon,
                                 config = "not a list"),
                 "'config': Must be of type 'list'")

    expect_error(exportUserRoles(rcon,
                                 api_param = list(1)),
                 "'api_param': Must have names")
    expect_error(exportUserRoles(rcon,
                                 api_param = "not a list"),
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# importUserRoles Argument Validation                            ####

test_that(
  "Return an error if rcon is not a redcapConnection",
  {
    local_reproducible_output(width = 200)
    expect_error(importUserRoles("not an rcon",
                                 data = redcapUserRoleStructure(rcon$version())),
                 "no applicable method for 'importUserRoles'")
  }
)

test_that(
  "Return an error if data is not a data frame",
  {
    local_reproducible_output(width = 200)
    expect_error(importUserRoles(rcon,
                                 data = "not a data frame"),
                 "'data': Must be of type 'data.frame'")

    expect_error(importUserRoles(rcon,
                                 data = mtcars),
                 "'names[(]data[)]'[:] Must be a subset of")
  }
)

test_that(
  "Return an error if consolidate is not logical(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(importUserRoles(rcon,
                                 data = redcapUserRoleStructure(rcon$version()),
                                 consolidate = "TRUE"),
                 "'consolidate': Must be of type 'logical'")
    expect_error(importUserRoles(rcon,
                                 data = redcapUserRoleStructure(rcon$version()),
                                 consolidate = c(TRUE, FALSE)),
                 "'consolidate': Must have length 1")
  }
)


test_that(
  "Validate config, api_param",
  {
    local_reproducible_output(width = 200)

    expect_error(importUserRoles(rcon,
                                 data = redcapUserRoleStructure(rcon$version()),
                                 config = list(1)),
                 "'config': Must have names")
    expect_error(importUserRoles(rcon,
                                 data = redcapUserRoleStructure(rcon$version()),
                                 config = "not a list"),
                 "'config': Must be of type 'list'")

    expect_error(importUserRoles(rcon,
                                 data = redcapUserRoleStructure(rcon$version()),
                                 api_param = list(1)),
                 "'api_param': Must have names")
    expect_error(importUserRoles(rcon,
                                 data = redcapUserRoleStructure(rcon$version()),
                                 api_param = "not a list"),
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# deleteUserRole Argument Validation                             ####

test_that(
  "Return an error if rcon is not a redcapConnection",
  {
    local_reproducible_output(width = 200)
    expect_error(deleteUserRoles("not an rcon",
                                 user_roles = "role name"),
                 "no applicable method for 'deleteUserRoles'")
  }
)

test_that(
  "Return an error if user_roles is not a character",
  {
    local_reproducible_output(width = 200)
    expect_error(deleteUserRoles(rcon,
                                 user_roles = 123),
                 "'user_roles': Must be of type 'character'")
  }
)

test_that(
  "exportUserRoles handles missing access columns",
  {
    mock_roles <- data.frame(unique_role_name = "role_1",
                             role_label = "Role 1",
                             user_rights = 1,
                             forms_export = "form_1:0",
                             stringsAsFactors = FALSE)

    mockery::stub(exportUserRoles.redcapApiConnection,
                  "makeApiCall",
                  function(...) mock_roles)

    UserRoles <- exportUserRoles(rcon,
                                 labels = TRUE,
                                 form_rights = FALSE)

    expect_equal(UserRoles$user_rights,
                 factor("Access", levels = c("No Access", "Access")))
    expect_false("design" %in% names(UserRoles))
  }
)


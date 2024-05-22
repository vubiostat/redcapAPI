context("User Role Assignment Methods Argument Validation")

#####################################################################
# exportUserRoleAssignments Argument Validation                  ####

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportUserRoleAssignments("not an rcon"), 
                 "no applicable method for 'exportUserRoleAssignments'")
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)
   
    expect_error(exportUserRoleAssignments(rcon, 
                                 config = list(1)), 
                 "'config': Must have names")
    expect_error(exportUserRoleAssignments(rcon, 
                                 config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportUserRoleAssignments(rcon, 
                                 api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportUserRoleAssignments(rcon, 
                                 api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# importUserRoleAssignments Argument Validation                  ####

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(importUserRoleAssignments("not an rcon", 
                                           data = REDCAP_USER_ROLE_ASSIGNMENT_STRUCTURE), 
                 "no applicable method for 'importUserRoleAssignments'")
  }
)

test_that(
  "Return an error if data is not a data frame", 
  {
    local_reproducible_output(width = 200)
    expect_error(importUserRoleAssignments(rcon, 
                                           data = "note a data frame"), 
                 "'data': Must be of type 'data.frame'")
    
    expect_error(importUserRoleAssignments(rcon, 
                                           data = data.frame(user = "a user name", 
                                                             role = "a role name")), 
                 "'names[(]data[)]': Must be a subset of")
    
    expect_error(importUserRoleAssignments(rcon, 
                                           data = data.frame(username = "not a user", 
                                                             unique_role_name = NA_character_)), 
                 "'data[$]username': Must be a subset of")
    
    expect_error(importUserRoleAssignments(rcon, 
                                           data = data.frame(username = "not a user", 
                                                             unique_role_name = "not a role")), 
                 "'data[$]unique_role_name': Must be a subset of")
  }
)

test_that(
  "Return an error if there are duplicate usernames", 
  {
    local_reproducible_output(width = 200)
    the_user <- rcon$users()$username[1]
    NewRole <- data.frame(unique_role_name = NA_character_, 
                           role_label = "Temporary role", 
                           stringsAsFactors = FALSE)
    importUserRoles(rcon, 
                    NewRole)
    
    the_role <- rcon$user_roles()$unique_role_name
    
    ImportAssignmentTest <-
      data.frame(username = rep(the_user, 2), 
                 unique_role_name = c(the_role, NA_character_),
                 stringsAsFactors = FALSE)
    
    expect_error(
      importUserRoleAssignments(rcon, 
                                data = ImportAssignmentTest), 
      "Each username may only be listed once"
    )
    
    deleteUserRoles(rcon, the_role)
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)

    expect_error(importUserRoleAssignments(rcon, 
                                           data = REDCAP_USER_ROLE_ASSIGNMENT_STRUCTURE,
                                           user_roles = "user-role",
                                           config = list(1)), 
                 "'config': Must have names")
    expect_error(importUserRoleAssignments(rcon, 
                                           data = REDCAP_USER_ROLE_ASSIGNMENT_STRUCTURE,
                                           user_roles = "user-role",
                                           config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(importUserRoleAssignments(rcon, 
                                           data = REDCAP_USER_ROLE_ASSIGNMENT_STRUCTURE,
                                           user_roles = "user-role",
                                           api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(importUserRoleAssignments(rcon, 
                                           data = REDCAP_USER_ROLE_ASSIGNMENT_STRUCTURE,
                                           user_roles = "user-role",
                                           api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

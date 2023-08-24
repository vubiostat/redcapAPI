context("User Role Assignment Methods Functionality")

importUsers(rcon, 
            data = data.frame(username = EXPENDABLE_USER))

test_that(
  "Import and Export of User Role Assignments",
  {
    skip_if(!RUN_USER_TESTS, 
            "User Assignment tests without an expendable user could have negative consequences and are not run")
    # we don't really want to run these until we have an expendable user. 
    # otherwise we run the risk of cutting off access a user running the
    # test suite would need to continue testing.
    the_user <- EXPENDABLE_USER

    # Make sure you include api_import and api_export rights in the
    # role so you don't lock someone out of testing.
    NewRole <- data.frame(unique_role_name = NA_character_,
                          role_label = "Temporary role",
                          api_import = 1,
                          api_export = 1,
                          user_rights = 1,
                          design = 1,
                          stringsAsFactors = FALSE)
    importUserRoles(rcon,
                    NewRole)

    rcon$refresh_user_roles()
    the_role <- rcon$user_roles()$unique_role_name

    ImportAssignmentTest <-
      data.frame(username = the_user,
                 unique_role_name = the_role,
                 stringsAsFactors = FALSE)

    expect_message(importUserRoleAssignments(rcon,
                                             data = ImportAssignmentTest),
                   "User-Role Assignments Added/Updated: 1")

    CompareFrame <- rcon$user_role_assignment()
    CompareFrame <- CompareFrame[CompareFrame$username == the_user, ]
    
    expect_equal(CompareFrame$unique_role_name, 
                 the_role)
    
    ImportAssignmentTest <-
      data.frame(username = the_user,
                 unique_role_name = NA_character_,
                 stringsAsFactors = FALSE)
    
    expect_message(importUserRoleAssignments(rcon, 
                                             data = ImportAssignmentTest),
                   "User-Role Assignments Added/Updated: 1")
    
    CompareFrame <- rcon$user_role_assignment()
    CompareFrame <- CompareFrame[CompareFrame$username == the_user, ]
    
    expect_true(is.na(CompareFrame$unique_role_name))

    expect_message(deleteUserRoles(rcon, the_role), 
                   "User Roles Deleted: 1")
  }
)


test_that(
  "Import User Changes when User is Assigned a Role", # Issue 206
  {
    UserRole <- data.frame(role_label = "Temporary Role", 
                           design = 1, 
                           user_rights = 1, 
                           reports = 1)
    
    importUserRoles(rcon, data = UserRole)
    
    THIS_ROLE <- rcon$user_roles()$unique_role_name[1]
    
    importUserRoleAssignments(rcon, 
                              data = data.frame(username = EXPENDABLE_USER, 
                                                unique_role_name = THIS_ROLE))
    
    importUsers(rcon, data = data.frame(username = EXPENDABLE_USER, 
                                        data_access_groups = 1))
    
    NewUser <- exportUsers(rcon, labels = FALSE)
    NewUser <- NewUser[NewUser$username == EXPENDABLE_USER, ]
    
    # The user is still assigned to the role, so the role permission should
    # be displayed (no access)
    expect_equal(NewUser$data_access_groups, 0)
    
    importUserRoleAssignments(rcon, 
                              data = data.frame(username = "bstat_api_user", 
                                                unique_role_name = NA_character_))
    
    NewUser <- exportUsers(rcon, labels = FALSE)
    NewUser <- NewUser[NewUser$username == EXPENDABLE_USER, ]
    
    # Now that the role has been unassigned, the permission we uploaded (access)
    # should be returned.
    expect_equal(NewUser$data_access_groups, 1)
    
    importUsers(rcon, data = data.frame(username = EXPENDABLE_USER, 
                                        data_access_groups = 0))
    
    deleteUserRoles(rcon, user_roles = THIS_ROLE) 
  }
)

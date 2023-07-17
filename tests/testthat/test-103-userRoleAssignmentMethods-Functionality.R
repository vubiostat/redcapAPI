context("User Role Assignment Methods Functionality")

# test_that(
#   "Import and Export of User Role Assignments", 
#   {
#     the_user <- rcon$users()$username[1]
#     
#     # Make sure you include api_import and api_export rights in the 
#     # role so you don't lock someone out of testing.
#     NewRole <- data.frame(unique_role_name = NA_character_, 
#                           role_label = "Temporary role", 
#                           api_import = 1, 
#                           api_export = 1, 
#                           user_rights = 1, 
#                           design = 1, 
#                           stringsAsFactors = FALSE)
#     importUserRoles(rcon, 
#                     NewRole)
#     
#     rcon$refresh_user_roles()
#     the_role <- rcon$user_roles()$unique_role_name
#     
#     ImportAssignmentTest <-
#       data.frame(username = the_user, 
#                  unique_role_name = the_role,
#                  stringsAsFactors = FALSE)
#     
#     expect_message(importUserRoleAssignments(rcon, 
#                                              data = ImportAssignmentTest), 
#                    "User-Role Assignments Added/Updated: 1")
#     
#     expect_equal()
#     
#   }
# )
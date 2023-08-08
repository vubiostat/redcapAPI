context("User Role Methods Functionality")

test_that(
  "User Role Methods Functionality", 
  {
    # Import a single user role
    rcon$flush_all()
    
    NewRole <- data.frame(role_label = "User Role 1", 
                          user_rights = 1)
  
    
    expect_message(importUserRoles(rcon, 
                                   data = NewRole), 
                   "User Roles Added/Modified: 1")
    
    # Verify that the user role was updated. 
    UserRoles <- exportUserRoles(rcon)
    
    
    # User roles, labeled
    expect_data_frame(UserRoles)
    expect_equal(UserRoles$user_rights, 
                 factor("Access", levels = c("No Access", "Access")))
    
    
    # User roles, unlabeled
    UserRoles <- exportUserRoles(rcon, 
                                 labels = FALSE)
    expect_data_frame(UserRoles)
    expect_equal(UserRoles$user_rights, 
                 1)
    
    
    # Update the existing user role
    UpdateRole <- data.frame(unique_role_name = UserRoles$unique_role_name[1], 
                             role_label = "User Role 1",
                             design = 1, 
                             reports = 1)
    
    expect_message(importUserRoles(rcon, 
                                   data = UpdateRole), 
                   "User Roles Added/Modified: 1")
    
    UserRoles <- exportUserRoles(rcon)
    
    expect_equal(as.character(UserRoles$design), 
                 "Access")
    expect_equal(as.character(UserRoles$reports), 
                 "Access")
    
    rcon$refresh_user_roles()
    rcon$user_roles()
    # Cleanup by deleting the user role
    expect_message(deleteUserRoles(rcon, UserRoles$unique_role_name[1]), 
                   "User Roles Deleted: 1")
  }
)
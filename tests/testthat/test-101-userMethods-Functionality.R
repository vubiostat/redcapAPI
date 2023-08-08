context("User Methods Functionality")

test_that(
  "Import / Export / Delete User Functionality", 
  {
    # skip_if(length(EXPENDABLE_USER) == 0, 
    #         "User tests without an expendable user could have negative consequences and are not run.")

    if (EXPENDABLE_USER %in% rcon$users()$username){
      deleteUsers(rcon, 
                  users = EXPENDABLE_USER)
    }
    
    # Import a user
    expect_message(importUsers(rcon, 
                               data = data.frame(username = EXPENDABLE_USER)), 
                   "Users Added/Modified: 1")
    
    # Verify the user was added
    expect_true(EXPENDABLE_USER %in% rcon$users()$username)
    
    # Modify the user permissions
    
    expect_message(importUsers(rcon, 
                               data = data.frame(username = EXPENDABLE_USER, 
                                                 alerts = 1)), 
                   "Users Added/Modified: 1")
    
    Users <- exportUsers(rcon)
    Users <- Users[rcon$users()$username %in% EXPENDABLE_USER, ]
    expect_true(Users$alerts %in% "Access")
    
    # Clean Up
    
    deleteUsers(rcon, 
                users = EXPENDABLE_USER)
  }
)

# FIXME: Add Tests for options in importUsers and exportUsers
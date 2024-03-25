context("User Methods Functionality")

test_that(
  "Import / Export Users", 
  {
    skip_if(!RUN_USER_TESTS,
            "User tests without an expendable user could have negative consequences and are not run.")

    if (EXPENDABLE_USER %in% rcon$users()$username){
      deleteUsers(rcon, 
                  users = EXPENDABLE_USER)
    }
    
    # Import a user
    n_imported <- importUsers(rcon, 
                              data = data.frame(username = EXPENDABLE_USER))
    expect_equal(n_imported, "1")
    # Verify the user was added
    expect_true(EXPENDABLE_USER %in% rcon$users()$username)
    
    # Modify the user permissions
    
    n_imported <- importUsers(rcon, 
                              data = data.frame(username = EXPENDABLE_USER, 
                                                alerts = 1)) 
    expect_equal(n_imported, "1")
    
    Users <- exportUsers(rcon)
    Users <- Users[rcon$users()$username %in% EXPENDABLE_USER, ]
    expect_true(Users$alerts %in% "Access")
  }
)

test_that(
  "Import Users Options", 
  {
    skip_if(!RUN_USER_TESTS,
            "User tests without an expendable user could have negative consequences and are not run.")
    
    if (EXPENDABLE_USER %in% rcon$users()$username){
      deleteUsers(rcon, 
                  users = EXPENDABLE_USER)
    }
    
    importUsers(rcon, 
                data = data.frame(username = EXPENDABLE_USER))
    
    expect_true(EXPENDABLE_USER %in% rcon$users()$username)
    
    
    # apply user permissions as character
    
    importUsers(rcon, 
                data = data.frame(username = EXPENDABLE_USER, 
                                  alerts = "Access"))
    
    Users <- rcon$users()
    Users <- Users[Users$username %in% EXPENDABLE_USER, ]
    expect_true(Users$alerts %in% "Access")
    
    importUsers(rcon, 
                data = data.frame(username = EXPENDABLE_USER, 
                                  alerts = "No Access"))
    
    Users <- rcon$users()
    Users <- Users[Users$username %in% EXPENDABLE_USER, ]
    expect_true(Users$alerts %in% "No Access")
    
    # Update form permissions with consolidated format
    
    importUsers(rcon, 
                data = data.frame(username = EXPENDABLE_USER, 
                                  data_export = 1,
                                  record_id_form_access = 1, 
                                  record_id_export_access = 1))
    
    Users <- exportUsers(rcon)
    Users <- Users[Users$username %in% EXPENDABLE_USER, ]
    expect_true(grepl("record_id:1",Users$forms))

    # Update form permissions with unconsolidated format
    
    importUsers(rcon, 
                data = data.frame(username = EXPENDABLE_USER,
                                  data_export = 1, 
                                  forms = c("record_id:0"), 
                                  forms_export = "record_id:0"), 
                consolidate = FALSE)
    
    Users <- exportUsers(rcon)
    Users <- Users[Users$username %in% EXPENDABLE_USER, ]
    expect_true(grepl("record_id:0",Users$forms))
  }
)



test_that(
  "Export User Options", 
  {
    skip_if(!RUN_USER_TESTS,
            "User tests without an expendable user could have negative consequences and are not run.")
    
    importUsers(rcon, 
                data = data.frame(username = EXPENDABLE_USER, 
                                  expiration = Sys.Date() + 365,
                                  data_export = 1,
                                  forms = "record_id:1"))
    
    Users <- exportUsers(rcon, dates = TRUE)
    expect_posixct(Users$expiration)
    
    Users <- exportUsers(rcon, dates = FALSE)
    expect_character(Users$expiration)
    
    Users <- exportUsers(rcon, labels = TRUE)
    expect_factor(Users$design, 
                  levels = c("No Access", "Access"))
    
    Users <- exportUsers(rcon, labels = FALSE)
    expect_integerish(Users$design)
    
    Users <- exportUsers(rcon, labels = TRUE, form_rights = TRUE)
    expect_true("record_id_form_access" %in% names(Users))
    expect_factor(Users$record_id_form_access)
    expect_true("record_id_export_access" %in% names(Users))
    expect_factor(Users$record_id_export_access)
    
    
    Users <- exportUsers(rcon, labels = FALSE, form_rights = TRUE)
    expect_true("record_id_form_access" %in% names(Users))
    expect_integerish(Users$record_id_form_access)
    expect_true("record_id_export_access" %in% names(Users))
    expect_integerish(Users$record_id_export_access)
    
    
    Users <- exportUsers(rcon, labels = TRUE, form_rights = FALSE)
    expect_false("record_id_form_access" %in% names(Users))
    expect_false("record_id_export_access" %in% names(Users))
  }
)



test_that(
  "Delete User Functionality", 
  {
    skip_if(!RUN_USER_TESTS,
            "User tests without an expendable user could have negative consequences and are not run.")

    # Clean Up
    n_deleted <- deleteUsers(rcon, 
                               users = EXPENDABLE_USER) 
    expect_equal(n_deleted, "1")
  }
)

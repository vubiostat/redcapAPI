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

    # ensure export > import > export equality
    # initial import fixes the state
    importUsers(rcon, rcon$users())
    Users <- exportUsers(rcon)
    importUsers(rcon, Users)
    # cache should be correct
    expect_equal(rcon$users(), exportUsers(rcon))
    expect_equal(rcon$users(), Users)

    # Modify the user permissions

    n_imported <- importUsers(rcon,
                              data = data.frame(username = EXPENDABLE_USER,
                                                alerts = 1))
    expect_equal(n_imported, "1")

    Users <- rcon$users()
    Users <- Users[Users$username %in% EXPENDABLE_USER, ]
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
    # leaving an instrument off implicitly sets permission to 0
                                  forms_export = ""),
                consolidate = FALSE)

    Users <- exportUsers(rcon)
    Users <- Users[Users$username %in% EXPENDABLE_USER, ]
    expect_true(grepl("record_id:0",Users$forms))
    expect_true(grepl("record_id:0",Users$forms_export))

    # consolidated format when consolidate is FALSE
    expect_warning(importUsers(rcon,
                data = data.frame(username = EXPENDABLE_USER,
                                  record_id_form_access = 1,
                                  forms = 'record_id:0',
                                  forms_export = ''),
                consolidate = FALSE))
    Users <- exportUsers(rcon)
    Users <- Users[Users$username %in% EXPENDABLE_USER, ]
    expect_true(grepl("record_id:0",Users$forms))
  }
)

test_that(
  "Import User DAG Assignments",
  {
    skip_if(!RUN_USER_TESTS,
            "User tests without an expendable user could have negative consequences and are not run.")

    if (EXPENDABLE_USER %in% rcon$users()$username){
      deleteUsers(rcon,
                  users = EXPENDABLE_USER)
    }

    importUsers(rcon,
                data = data.frame(username = EXPENDABLE_USER))

    # create temporary DAG; it probably already exists at this point
    TmpDag <- !'test_dag_1' %in% exportDags(rcon)$unique_group_name
    if(TmpDag) {
      NewDag <- data.frame(data_access_group_name = 'test_dag_1',
                           unique_group_name = NA_character_)
      importDags(rcon, data = NewDag)
    }

    # Update data_access_group to a legitimate DAG
    Users <- exportUsers(rcon)
    Users <- Users[Users$username %in% EXPENDABLE_USER, ]
    Users[,'data_access_group'] <- 'test_dag_1'
    importUsers(rcon, data = Users)
    DagAsgmt <- exportUserDagAssignments(rcon)
    expect_equal('test_dag_1',
                 DagAsgmt[DagAsgmt[,'username'] == EXPENDABLE_USER, 'redcap_data_access_group'])

    # Update data_access_group to "No Assignment"
    Users[,'data_access_group'] <- NA_character_
    # warning indicates this gives view access to all records
    expect_warning(importUsers(rcon, data = Users), 'view all records')
    DagAsgmt <- exportUserDagAssignments(rcon)
    expect_true(is.na(DagAsgmt[DagAsgmt[,'username'] == EXPENDABLE_USER, 'redcap_data_access_group']))

    # Try a bad DAG
    Users[,'data_access_group'] <- 'uncouth_dag'
    expect_error(importUsers(rcon, data = Users))

    if(TmpDag) {
      deleteDags(rcon, 'test_dag_1')
    }
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

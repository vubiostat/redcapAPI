context("redcapConnection Caching")

API_KEY <- rcon$token

#####################################################################
# Caching tests                                                  ####

test_that(
  "Caching of metadata", 
  {
    rcon$flush_all()
    expect_false(rcon$has_metadata())
    
    rcon$metadata()
    expect_true(rcon$has_metadata())
    
    rcon$flush_metadata()
    expect_false(rcon$has_metadata())
    
    rcon$refresh_metadata()
    expect_true(rcon$has_metadata())
  }
)

test_that(
  "caching of arms", 
  {
    rcon$flush_all()
    expect_false(rcon$has_arms())
    
    rcon$arms()
    expect_true(rcon$has_arms())
    
    rcon$flush_arms()
    expect_false(rcon$has_arms())
    
    rcon$refresh_arms()
    expect_true(rcon$has_arms())
  }
)

test_that(
  "caching of events", 
  {
    rcon$flush_all()
    expect_false(rcon$has_events())
    
    rcon$events()
    expect_true(rcon$has_events())
    
    rcon$flush_events()
    expect_false(rcon$has_events())
    
    rcon$refresh_events()
    expect_true(rcon$has_events())
  }
)

test_that(
  "caching of instruments", 
  {
    rcon$flush_all()
    expect_false(rcon$has_instruments())
    
    rcon$instruments()
    expect_true(rcon$has_instruments())
    
    rcon$flush_instruments()
    expect_false(rcon$has_instruments())
    
    rcon$refresh_instruments()
    expect_true(rcon$has_instruments())
  }
)

test_that(
  "caching of fieldnames", 
  {
    rcon$flush_all()
    expect_false(rcon$has_fieldnames())
    
    rcon$fieldnames()
    expect_true(rcon$has_fieldnames())
    
    rcon$flush_fieldnames()
    expect_false(rcon$has_fieldnames())
    
    rcon$refresh_fieldnames()
    expect_true(rcon$has_fieldnames())
  }
)

test_that(
  "caching of mapping", 
  {
    rcon$flush_all()
    expect_false(rcon$has_mapping())
    
    rcon$mapping()
    expect_true(rcon$has_mapping())
    
    rcon$flush_mapping()
    expect_false(rcon$has_mapping())
    
    rcon$refresh_mapping()
    expect_true(rcon$has_mapping())
  }
)

test_that(
  "caching of repeatInstrumentEvent", 
  {
    local_reproducible_output(width = 200)
    
    rcon$flush_all()
    expect_false(rcon$has_repeatInstrumentEvent())
    
    rcon$repeatInstrumentEvent()
    expect_true(rcon$has_repeatInstrumentEvent())
    
    rcon$flush_repeatInstrumentEvent()
    expect_false(rcon$has_repeatInstrumentEvent())
    
    rcon$refresh_repeatInstrumentEvent()
    expect_true(rcon$has_repeatInstrumentEvent())
  }
)

test_that(
  "caching of users", 
  {
    rcon$flush_all()
    expect_false(rcon$has_users())
    
    rcon$users()
    expect_true(rcon$has_users())
    
    rcon$flush_users()
    expect_false(rcon$has_users())
    
    rcon$refresh_users()
    expect_true(rcon$has_users())
  }
)

test_that(
  "caching of user roles", 
  {
    rcon$flush_all()
    expect_false(rcon$has_user_roles())
    
    rcon$user_roles()
    expect_true(rcon$has_user_roles())
    
    rcon$flush_user_roles()
    expect_false(rcon$has_user_roles())
    
    rcon$refresh_user_roles()
    expect_true(rcon$has_user_roles())
  }
)

test_that(
  "caching of user role assignments", 
  {
    rcon$flush_all()
    expect_false(rcon$has_user_role_assignment())
    
    rcon$user_role_assignment()
    expect_true(rcon$has_user_role_assignment())
    
    rcon$flush_user_role_assignment()
    expect_false(rcon$has_user_role_assignment())
    
    rcon$refresh_user_role_assignment()
    expect_true(rcon$has_user_role_assignment())
  }
)

test_that(
  "caching of Data Access Groups", 
  {
    local_reproducible_output(width = 200)
    
    rcon$flush_all()
    expect_false(rcon$has_dags())
    
    rcon$dags()
    expect_true(rcon$has_dags())
    
    rcon$flush_dags()
    expect_false(rcon$has_dags())
    
    rcon$refresh_dags()
    expect_true(rcon$has_dags())
  }
)

test_that(
  "caching of Data Access Group Assignments", 
  {
    local_reproducible_output(width = 200)
    
    rcon$flush_all()
    expect_false(rcon$has_dag_assignment())
    
    rcon$dag_assignment()
    expect_true(rcon$has_dag_assignment())
    
    rcon$flush_dag_assignment()
    expect_false(rcon$has_dag_assignment())
    
    rcon$refresh_dag_assignment()
    expect_true(rcon$has_dag_assignment())
  }
)

test_that(
  "caching of projectInformation", 
  {
    rcon$flush_all()
    expect_false(rcon$has_projectInformation())
    
    rcon$projectInformation()
    expect_true(rcon$has_projectInformation())
    
    rcon$flush_projectInformation()
    expect_false(rcon$has_projectInformation())
    
    rcon$refresh_projectInformation()
    expect_true(rcon$has_projectInformation())
  }
)

test_that(
  "caching of version", 
  {
    rcon$flush_all()
    expect_false(rcon$has_version())
    
    rcon$version()
    expect_true(rcon$has_version())
    
    rcon$flush_version()
    expect_false(rcon$has_version())
    
    rcon$refresh_version()
    expect_true(rcon$has_version())
  }
)



test_that(
  "caching of fileRepository", 
  {
    rcon$flush_all()
    expect_false(rcon$has_fileRepository())
    
    rcon$fileRepository()
    expect_true(rcon$has_fileRepository())
    
    rcon$flush_fileRepository()
    expect_false(rcon$has_fileRepository())
    
    rcon$refresh_fileRepository()
    expect_true(rcon$has_fileRepository())
  }
)

test_that(
  "caching of externalCoding", 
  {
    rcon$flush_all()
    expect_false(rcon$has_externalCoding())
    
    rcon$externalCoding()
    expect_true(rcon$has_externalCoding())
    
    rcon$flush_externalCoding()
    expect_false(rcon$has_externalCoding())
    
    rcon$refresh_externalCoding()
    expect_true(rcon$has_externalCoding())
  }
)





test_that(
  "retrieve and set retries", 
  {
    expect_equal(rcon$retries(), 5)
    expect_error(rcon$set_retries("3"))
    
    rcon$set_retries(4)
    expect_equal(rcon$retries(), 4)
    rcon$set_retries(5)
  }
)

test_that(
  "retrieve and set retry_interval", 
  {
    expect_equal(rcon$retry_interval(), 
                 c(2^(1:5)))
    
    expect_error(rcon$set_retry_interval(-3))
    
    rcon$set_retry_interval(3)
    expect_equal(rcon$retry_interval(), 
                 rep(3, 5))
    
    rcon$set_retry_interval(2^(1:5))
  }
)

test_that(
  "retrieve and set retry_quietly", 
  {
    expect_true(rcon$retry_quietly())
    
    expect_error(rcon$set_retry_quietly("FALSE"))
    
    rcon$set_retry_quietly(FALSE)
    expect_false(rcon$retry_quietly())
    
    rcon$set_retry_quietly(TRUE)
  }
)

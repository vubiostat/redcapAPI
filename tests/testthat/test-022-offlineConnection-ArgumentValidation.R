context("offlineConnection Argument Validation")

#####################################################################
# Argument Validations

test_that(
  "Validate arguments", 
  {
    local_reproducible_output(width = 200)
    expect_error(offlineConnection(meta_data = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(arms = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(events = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(instruments = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(field_names = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(mapping = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(repeat_instrument = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(users = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(user_roles = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(user_role_assignment = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(dags = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(dag_assignment = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(project_info = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(version = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(file_repo = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')")
    expect_error(offlineConnection(records = 123), 
                 "(Must be of type 'character'|Must be of type 'data.frame')") 
    expect_error(offlineConnection(external_coding = 123), 
                 "(Must be of type 'character'|Must be of type 'list')")
  }
)


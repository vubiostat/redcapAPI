context("Export Survey Link Functionality")

# There is no way to designate a form to be a survey via the API.
# Thus, testing that survey related fields are downloaded would require
#   manual intervention by a user to configure a form to be a survey.
# We prefer not to require such intervention in the test suite, and so
#   these tests will not be written until such a time that surveys
#   can be enabled and configured through the API.
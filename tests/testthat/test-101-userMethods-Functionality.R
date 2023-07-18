context("User Methods Functionality")

# FIXME: Add Tests

test_that(
  "Import / Export / Delete User Functionality", 
  {
    skip_if(length(EXPENDABLE_USER) == 0, 
            "User tests without an expendable user could have negative consequences and are not run.")
  }
)
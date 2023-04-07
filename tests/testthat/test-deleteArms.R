context("deleteArms.R")

#####################################################################
# Functional Tests

# FIXME: Write tests that work with importArms

#####################################################################
# Argument Validation

test_that(
  "Return an error when rcon is not a redcapApiConnection", 
  {
    expect_error(deleteArms(rcon = "not an rcon"), 
                 "no applicable method for 'deleteArms'")
  }
)

# This didn't throw an error and deleted a lot of stuff. 
# Be careful!
# test_that(
#   "Return an error when arms is not integerish", 
#   {
#     expect_error(deleteArms(rcon = rcon, 
#                             arms = c("1", "2")))
#   }
# )
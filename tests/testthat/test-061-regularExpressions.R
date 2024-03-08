context("Regular Expressions")

#####################################################################
# REGEX_FIELD_NAME                                               ####

test_that(
  "REGEX_FIELD_NAME correctly identifies acceptable field names", 
  {
    field_name <- c("a", "ab", "a1", "a_1", 
                    "a_", "_a", "a__1", "1", "1a", 
                    "multiple_under_scores")
    
    expect_equal(grepl(REGEX_FIELD_NAME, field_name, perl = TRUE), 
                 c(TRUE, TRUE, TRUE, TRUE, 
                   FALSE, FALSE, FALSE, FALSE, FALSE, 
                   TRUE))
  }
)

#####################################################################
# REGEX_FORM_NAME                                                ####

test_that(
  "REGEX_FORM_NAME correctly identifies acceptable field names", 
  {
    form_name <- c("a", "ab", "a1", "a_1", 
                    "a_", "_a", "a__1", "1", "1a", 
                    "multiple_under_scores")
    
    expect_equal(grepl(REGEX_FORM_NAME, form_name, perl = TRUE), 
                 c(TRUE, TRUE, TRUE, TRUE, 
                   FALSE, FALSE, FALSE, FALSE, FALSE, 
                   TRUE))
  }
)
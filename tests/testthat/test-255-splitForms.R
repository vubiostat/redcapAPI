context("splitForms.R")

Records <- exportRecordsTyped(rcon)

#####################################################################
# Argument Validation                                            ####

test_that(
  "splitForms argument Validation", 
  {
    local_reproducible_output(width = 200)
    # Error if Records is not a data frame
    expect_error(splitForms(letters, rcon), 
                 "Variable 'Records': Must be of type 'data.frame'")
    
    # Error if rcon is not a redcapConnection
    expect_error(splitForms(Records, 
                            rcon = letters), 
                 "Variable 'rcon': Must inherit from class 'redcapConnection'")
    
    
    # Error if envir is not an environment when not NULL
    expect_error(splitForms(Records, 
                            rcon, 
                            envir = letters), 
                 "Variable 'envir': Must be an environment")
    
    # Error if base is not a character(1)
    expect_error(splitForms(Records, 
                            rcon, 
                            base = pi), 
                 "Variable 'base': Must be of type 'character'")
    
    # Error if post is not a function
    expect_error(splitForms(Records, 
                            rcon, 
                            post = pi), 
                 "Variable 'post': Must be a function")
  }
)

#####################################################################
# Functional Behavior                                            ####

test_that(
  "splitForms Functional Behavior", 
  {
    not_record_id <- sort(unique(rcon$metadata()$form_name))
    not_record_id <- not_record_id[!not_record_id %in% "record_id"]
    
    # With no environment, the result is returned as a list
    this_split <- splitForms(Records, 
                             rcon)
    expect_list(this_split)
    expect_equal(sort(names(this_split)), 
                 not_record_id)
    
    # Apply [base] to the form name
    this_split <- splitForms(Records, 
                             rcon, 
                             base = "form")
    expect_equal(sort(names(this_split)), 
                 sprintf("form.%s", 
                         not_record_id))
    
    # Run the function in post processing
    this_split <- splitForms(Records, 
                             rcon, 
                             post = function(Recs, Rcon) as.matrix(Recs))
    expect_true(all(vapply(this_split, is.matrix, logical(1))))
    
    # Add to the desired environment
    split_env <- new.env()
    
    splitForms(Records, 
               rcon, 
               envir = split_env)
    
    expect_equal(ls(envir = split_env), 
                 sort(names(this_split)))
  }
)

test_that(
  "Carry over invalid attributes, remove rows of all missing data", 
  {
    TheseRecords <- suppressWarnings(exportRecordsTyped(rcon, 
                                       fields = c("date_mdy_test", "dropdown_test"), 
                                       validation = list(dropdown = function(x, field_name, coding) x != "1")))
    
    Split <- splitForms(TheseRecords, 
                        rcon)
    
    expect_data_frame(attr(Split$multiple_choice, "invalid"))
    
    expect_true(all(!is.na(Split$multiple_choice$dropdown_test)))
  }
)

test_that(
  "Split forms post process works",
  {
    this_split <-
      splitForms(
        Records, 
        rcon,
        post=function(Records, rcon) {assign("b", 2, pos=1)})
    expect_equal(b, 2)
    rm(b, pos=1)
  }
)

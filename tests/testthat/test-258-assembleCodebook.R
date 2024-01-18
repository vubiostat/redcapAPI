#####################################################################
# Argument Validation                                            ####

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    expect_error(assembleCodebook("not an rcon"), 
                 "no applicable method for 'assembleCodebook'")
  }
)

test_that(
  "Return an error if fields is not character", 
  {
    expect_error(assembleCodebook(rcon, 
                                  fields = 123), 
                 "'fields': Must be of type 'character'")
    
    expect_error(assembleCodebook(rcon, 
                                  fields = "not a field"), 
                 "'fields': Must be a subset of")
  }
)

test_that(
  "Return an error if forms is not character", 
  {
    expect_error(assembleCodebook(rcon, 
                                  forms = 123), 
                 "'forms': Must be of type 'character'")
    
    expect_error(assembleCodebook(rcon, 
                                  forms = "not a form"), 
                 "'forms': Must be a subset of")
  }
)

test_that(
  "Return an error if drop_fields is not character", 
  {
    expect_error(assembleCodebook(rcon, 
                                  drop_fields = 123), 
                 "'drop_fields': Must be of type 'character'")
    
    expect_error(assembleCodebook(rcon, 
                                  drop_fields = "not a field"), 
                 "'drop_fields': Must be a subset of")
  }
)

test_that(
  "Return an error if field_types is not character", 
  {
    expect_error(assembleCodebook(rcon, 
                                  field_types = 123), 
                 "'field_types': Must be of type 'character'")
  }
)

test_that(
  "Return an error if include_form_complete is not logical(1)", 
  {
    expect_error(assembleCodebook(rcon, 
                                  include_form_complete = "TRUE"), 
                 "'include_form_complete': Must be of type 'logical'")
    
    expect_error(assembleCodebook(rcon, 
                                  include_form_complete = c(TRUE, FALSE)), 
                 "'include_form_complete': Must have length 1")
  }
)

test_that(
  "Return an error if expand_check is not logical(1)", 
  {
    expect_error(assembleCodebook(rcon, 
                                  expand_check = "TRUE"), 
                 "'expand_check': Must be of type 'logical'")
    
    expect_error(assembleCodebook(rcon, 
                                  expand_check = c(TRUE, FALSE)), 
                 "'expand_check': Must have length 1")
  }
)

#####################################################################
# Functionality                                                  ####

test_that(
  "Return codebook objects", 
  {
    expect_data_frame(assembleCodebook(rcon))
    
    # For selected field names
    Codebook <- assembleCodebook(rcon, 
                                 fields = c("record_id", "text_test", 
                                            "dropdown_test"))
    expect_true(all(Codebook$field_name %in% c("record_id", 
                                               "text_test", 
                                               "dropdown_test", 
                                               "redcap_event_name", 
                                               "redcap_data_access_group", 
                                               "redcap_repeat_instrument", 
                                               "redcap_repeat_instance")))
    
    # For selected forms
    Codebook <- assembleCodebook(rcon, 
                                 forms = "numbers")
    expect_true(all(Codebook$form_name %in% c("record_id", "numbers", 
                                              "System Fields")))
    
    # Includes form_complete fields
    
    form_complete <- rcon$instruments()$instrument_name
    form_complete <- sprintf("%s_complete", form_complete)
    Codebook <- assembleCodebook(rcon)
    expect_true(all(form_complete %in% Codebook$field_name))
    
    # For selected field types
    Codebook <- assembleCodebook(rcon, 
                                 field_types = c("radio", "dropdown", "calc"))
    expect_true(all(Codebook$field_type %in% c("text", "dropdown", "radio", "calc", 
                                               "system_field")))
  }
)

test_that(
  "Codebook objects can be converted to lists", 
  {
    Codebook <- assembleCodebook(rcon)
    
    expect_list(as.list(Codebook))
  }
)

# Additional tests to write
# Turns out we can't write these tests because we can't import 
# missing data codes.  They have to be set manually.  
# Validation of these items must be performed manually
# 
# 1. Missing value codes added to checkbox variables
# 2. Missing value codes not added to form_complete fields
# 3. Missing value codes not added to system fields
# 3. Missing value codes added to all other field types properly

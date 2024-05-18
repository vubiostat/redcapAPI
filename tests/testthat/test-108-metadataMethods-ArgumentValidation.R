context("Meta Data Methods Argument Validation")

#####################################################################
# exportMetaData

test_that(
  "Return an error when rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportMetaData("not an rcon"), 
                 "no applicable method for 'exportMetaData'")
  }
)

test_that(
  "Return an error when fields is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportMetaData(rcon, fields = 1:3), 
                 "'fields'[:] Must be of type 'character'")
    
    expect_error(exportMetaData(rcon, 
                                fields = "not_a_field"), 
                 "'fields': Must be a subset of")
  }
)

test_that(
  "Return an error when forms is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportMetaData(rcon, forms = 1:3), 
                 "'forms'[:] Must be of type 'character'")
    expect_error(exportMetaData(rcon, forms = "not_a_form"), 
                 "'forms': Must be a subset of")
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)
   
    expect_error(exportMetaData(rcon, 
                                config = list(1)), 
                 "'config': Must have names")
    expect_error(exportMetaData(rcon, 
                                config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportMetaData(rcon, 
                                api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportMetaData(rcon, 
                                api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

#####################################################################
# importMetaData

test_that(
  "Return an error when rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(importMetaData("not an rcon"), 
                 "no applicable method for 'importMetaData'")
  }
)

test_that(
  "Return an error when data is not a data frame", 
  {
    local_reproducible_output(width = 200)
    expect_error(importMetaData(rcon, 
                                "not a data frame"))
  }
)

test_that(
  "Return an error when field_types is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(importMetaData(rcon, 
                                data = REDCAP_METADATA_STRUCTURE, 
                                field_types = 1:3))
  }
)

test_that(
  "Return an error when validation_types is not character", 
  {
    local_reproducible_output(width = 200)
    expect_error(importMetaData(rcon, 
                                data = REDCAP_METADATA_STRUCTURE, 
                                validation_types = 1:3))
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)

    expect_error(importMetaData(rcon, 
                                data = REDCAP_METADATA_STRUCTURE,
                                config = list(1)), 
                 "'config': Must have names")
    expect_error(importMetaData(rcon, 
                                data = REDCAP_METADATA_STRUCTURE,
                                config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(importMetaData(rcon, 
                                data = REDCAP_METADATA_STRUCTURE,
                                api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(importMetaData(rcon, 
                                data = REDCAP_METADATA_STRUCTURE,
                                api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)

# Validating content of the data frame

test_that(
  "Field names are valid", 
  {
    local_reproducible_output(width = 200)
    fields <- c("a_field_name",     #TRUE
                "a_field_name2",    #TRUE - trailing digits permitted
                "a_field123_name",  #TRUE - digits permitted in the name
                "A_field_name",    #FALSE - capital letters not permitted
                "a_Field_name",    #FALSE - captial letterss not permitted
                "1_field_name",    #FALSE - leading digits not permitted
                "_field_name",     #FALSE - leading underscore not permitted
                "a_field_name_",   #FALSE - trailing underscore not permitted
                "a field name",    #FALSE - spaces not permitted
                "a-field_name",    #FALSE - only digits, lowercase letters, and underscore permitted
                "a.field.name",    #FALSE - only digits, lowercase letters, and underscore permitted
                "a__field_name",   #FALSE - no consecutive underscores
                "a_fie___ld_name") #FALSE - no consecutive underscores

    
    expect_equal(isValidFieldName(fields), 
                 rep(c(TRUE, FALSE), c(3, 10)))
    
    coll <- checkmate::makeAssertCollection()
    
    isValidFieldName(c("_field_name", "field_name_", "field_name"), 
                     coll = coll)
    
    expect_error(checkmate::reportAssertions(coll), 
                 "field names do not conform to REDCap field name standards: [{]_field_name, field_name_[}]")
  }
)

test_that(
  "Form names are valid", 
  {
    local_reproducible_output(width = 200)
    forms <- c("a_form_name",      #TRUE
               "a_form_name2",    #TRUE - trailing digits permitted
               "a_form123_name",  #TRUE - digits permitted in the name
               "A_form_name",    #FALSE - capital letters not permitted
               "a_forM_name",    #FALSE - captial letters not permitted
               "1_form_name",    #FALSE - leading digits not permitted
               "_form_name",     #FALSE - leading underscore not permitted
               "a_form_name_",   #FALSE - trailing underscore not permitted
               "a form name",    #FALSE - spaces not permitted
               "a-form_name",    #FALSE - only digits, lowercase letters, and underscore permitted
               "a.form.name",    #FALSE - only digits, lowercase letters, and underscore permitted
               "a__form_name",   #FALSE - no consecutive underscores
               "a_fo___rm_name") #FALSE - no consecutive underscores
    
    
    expect_equal(isValidFormName(forms), 
                 rep(c(TRUE, FALSE), c(3, 10)))
    
    coll <- checkmate::makeAssertCollection()
    
    isValidFormName(c("_form_name", "form_name_", "form_name"), 
                     coll = coll)
    
    expect_error(checkmate::reportAssertions(coll), 
                 "form names do not conform to REDCap form name standards: [{]_form_name, form_name_[}]")
  }
)

test_that(
  "Field types are valid", 
  {
    local_reproducible_output(width = 200)
    types <- c("calc", 
               "checkbox", 
               "descriptive", 
               "dropdown", 
               "file", 
               "notes", 
               "radio", 
               "slider", 
               "text", 
               "truefalse", 
               "yesno", 
               "something_else", 
               "YESNO")
    
    coll <- checkmate::makeAssertCollection()
    
    result <- isValidFieldType(field_type = types, 
                               acceptable_field_types = REDCAP_METADATA_FIELDTYPE, 
                               coll = coll)
    
    expect_equal(result, 
                 rep(c(TRUE, FALSE), c(11, 2)))
    
    expect_error(checkmate::reportAssertions(coll), 
                 "The following field types are not valid field types: [{]something_else, YESNO[}]")
    
    flip_result <- isValidFieldType(field_type = types, 
                                    acceptable_field_types = c("something_else", "YESNO"))
    
    expect_equal(flip_result, 
                 rep(c(FALSE, TRUE), c(11, 2)))
  }
)

test_that(
  "Field validation types are valid", 
  {
    local_reproducible_output(width = 200)
    types <- c("zipcode", "signature", "number", "phone", "something_else", "date_dmy", NA)
    
    expect_equal(isValidFieldValidationType(types), 
                 c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE))
    
    expect_equal(isValidFieldValidationType(types, 
                                            allow_na = FALSE), 
                 c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE))
    
    expect_equal(isValidFieldValidationType(types, 
                                            validation_types = "something_else"), 
                 c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE))
    
    coll <- checkmate::makeAssertCollection()
    
    isValidFieldValidationType(types, 
                               coll = coll)
    
    expect_error(checkmate::reportAssertions(coll), 
                 "The following 'text_validation_type_or_show_slider_number' values are not valid [{]something_else[}]")
  }
)

test_that(
  "Choice fields are valid", 
  {
    local_reproducible_output(width = 200)
    # Remember, this only checks that syntax is correct. It does not check that syntax should be there.
    field_name <- c("drop_down_field", "radio_field", "checkbox_field", "slider_field", "number_field", "date_field")
    field_type <- c("dropdown",        "radio",       "checkbox",        "slider",      "text",         "date_dmy")
    choices <- c("1,A|2,B|3,C", "1,A|2,B|3,C", "1,A|2,B|3,C", "0|5|10", NA, "0|1|2")
    
    expect_true(all(isValidChoiceField(field_name = field_name, 
                                       field_type = field_type, 
                                       choices = choices)))
    
    # Now lets test some incorrect syntax
    
    field_name <- c("drop1", "drop2", "drop3", "drop4", "drop5")
    field_type <- c("dropdown", "dropdown", "dropdown", "dropdown", "dropdown")
    choices <- c("|1,A|2,B", 
                 "1;A|2;B", 
                 "1,A B | 2,C", 
                 "1,A |    2,B", 
                 "1,A | 2,B |")
    
    coll <- checkmate::makeAssertCollection()
    
    bad_drop <- isValidChoiceField(field_name = field_name, 
                                   field_type = field_type, 
                                   choices = choices, 
                                   coll = coll)
    expect_equal(bad_drop, 
                 c(FALSE, FALSE, TRUE, TRUE, FALSE))
    
    expect_error(checkmate::reportAssertions(coll), 
                 "The following fields have invalid 'select_choices_or_calculations': [{]drop1, drop2, drop5[}]")
    
    # And last, let's check sliders
    coll <- checkmate::makeAssertCollection()
    field_name <- c("slide1", "slide2", "slide3", "slide4", "slide5")
    field_type <- c("slider", "slider", "slider", "slider", "slider")
    choices <- c("||",
                 "   |  |      ",
                 " |1|10", 
                 "|a|10",
                 "1|2")
    
    bad_slide <- isValidChoiceField(field_name = field_name, 
                                    field_type = field_type, 
                                    choices = choices, 
                                    coll = coll)
    
    expect_equal(bad_slide, 
                 c(TRUE, TRUE, TRUE, TRUE, FALSE))
    
    expect_error(checkmate::reportAssertions(coll), 
                 "The following fields have invalid 'select_choices_or_calculations': [{]slide5[}]")
  }
)

test_that(
  "Values are set only on appropriate fields (validation)", 
  {
    local_reproducible_output(width = 200)
    coll <- checkmate::makeAssertCollection()
    
    field_name <- c("drop_down_field", "radio_field", "checkbox_field", "slider_field", "number_field", "date_field")
    field_type <- c("dropdown",        "radio",       "checkbox",        "slider",      "text",         "date_dmy")
    property <- c("1,A|2,B|3,C", "1,A|2,B|3,C", "1,A|2,B|3,C", "0|5|10", NA, "0|1|2")
    
    result <- .isPropertyOnAppropriateField(field_name = field_name, 
                                            field_type = field_type, 
                                            permissible_field_type = c("dropdown", "radio", "checkbox", "slider"), 
                                            property = property, 
                                            property_name = "text_validation_type_or_show_slider_number", 
                                            coll = coll)
    
    expect_equal(result, 
                 c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))
    
    expect_error(checkmate::reportAssertions(coll), 
                 "The following fields should not have a value in 'text_validation_type_or_show_slider_number': [{]date_field[}]")
  }
)

test_that(
  "Values are set only on appropriate fields (choices)", 
  {
    local_reproducible_output(width = 200)
    coll <- checkmate::makeAssertCollection()
    
    field_name <- c("calc", "drop_down_field", "radio_field", "checkbox_field", "slider_field", "number_field", "date_field")
    field_type <- c("calc", "dropdown",        "radio",       "checkbox",        "slider",      "text",         "date_dmy")
    property <- c("[field1] + [field2]", "1,A|2,B|3,C", "1,A|2,B|3,C", "1,A|2,B|3,C", "0|5|10", NA, "0|1|2")
    
    result <- .isPropertyOnAppropriateField(field_name = field_name, 
                                            field_type = field_type, 
                                            permissible_field_type = c("calc", "dropdown", "radio", "checkbox", "slider"), 
                                            property = property, 
                                            property_name = "select_choices_or_calculations", 
                                            coll = coll)
    
    expect_equal(result, 
                 c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))
    
    expect_error(checkmate::reportAssertions(coll), 
                 "The following fields should not have a value in 'select_choices_or_calculations': [{]date_field[}]")
  }
)

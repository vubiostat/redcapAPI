context("recastRecords.R")

fields <- c("dropdown_test",
            "radio_test",
            "checkbox_test",
            "yesno_test",
            "truefalse_test")

export_fields <- c("dropdown_test",
                   "radio_test",
                   "checkbox_test___x",
                   "checkbox_test___y",
                   "checkbox_test___z",
                   "yesno_test",
                   "truefalse_test")

Records <- exportRecordsTyped(rcon,
                              fields = fields)

#####################################################################
# Functionality                                                  ####

test_that(
  "Recast with defaults performs no changes",
  {
    Recast <- recastRecords(Records,
                            rcon = rcon,
                            fields = names(Records))

    expect_true(identical(Records, Recast))
  }
)

test_that(
  "Successfully recast back to raw",
  {
    Recast <- recastRecords(Records,
                            rcon = rcon,
                            fields = names(Records),
                            cast = list(dropdown = castRaw,
                                        radio = castRaw,
                                        checkbox = castRaw,
                                        yesno = castRaw,
                                        truefalse = as.numeric))

    expect_equal(sort(unique(Recast$dropdown_test)),     c(1, 2, 3))
    expect_equal(sort(unique(Recast$radio_test)),        c("a", "b", "c"))
    expect_equal(sort(unique(Recast$checkbox_test___x)), c(0, 1))
    expect_equal(sort(unique(Recast$checkbox_test___y)), c(0, 1))
    expect_equal(sort(unique(Recast$checkbox_test___z)), c(0, 1))
    expect_equal(sort(unique(Recast$yesno_test)),        c(0, 1))
    expect_equal(sort(unique(Recast$truefalse_test)),    c(0, 1))
  }
)

test_that(
  "Handle numeric indexing appropriately",
  {
    local_reproducible_output(width = 200)
    Recast <- recastRecords(Records,
                            rcon = rcon,
                            fields = c(8, 9, 5),
                            cast = list(dropdown = castRaw,
                                        radio = castRaw,
                                        checkbox = castRaw,
                                        yesno = castRaw,
                                        truefalse = as.numeric))

    expect_equal(sort(unique(Recast$dropdown_test)),     c(1, 2, 3))
    expect_equal(sort(unique(Recast$radio_test)),        c("a", "b", "c"))
    expect_equal(sort(unique(Recast$checkbox_test___x)), c(0, 1))
    expect_equal(levels(Recast$checkbox_test___y),       levels(Records$checkbox_test___y))
    expect_equal(levels(Recast$checkbox_test___z),       levels(Records$checkbox_test___z))
    expect_equal(levels(Recast$yesno_test),              c("No", "Yes"))
    expect_true(is.logical(Recast$truefalse_test))

    Recast <- recastRecords(Records,
                            rcon = rcon,
                            fields = c(9, 6, 11),
                            cast = list(dropdown = castRaw,
                                        radio = castRaw,
                                        checkbox = castRaw,
                                        yesno = castRaw,
                                        truefalse = as.numeric))

    expect_equal(levels(Recast$dropdown_test),           levels(Records$dropdown_test))
    expect_equal(sort(unique(Recast$radio_test)),        c("a", "b", "c"))
    expect_equal(levels(Recast$checkbox_test___x),       levels(Records$checkbox_test___x))
    expect_equal(sort(unique(Recast$checkbox_test___y)), c(0, 1))
    expect_equal(levels(Recast$checkbox_test___z),       levels(Records$checkbox_test___z))
    expect_equal(sort(unique(Recast$yesno_test)),        c(0, 1))
    expect_true(is.logical(Recast$truefalse_test))

    expect_error(recastRecords(Records,
                               rcon = rcon,
                               fields = c(11:17)),
                 "Columns [{]12, 13, 14, 15, 16, 17[}] requested in a data frame with 11 columns")
  }
)

test_that(
  "Handle logical indexing appropriately",
  {
    local_reproducible_output(width = 200)
    to_recast <- logical(ncol(Records))
    to_recast[c(8, 9, 5)] <- TRUE
    Recast <- recastRecords(Records,
                            rcon = rcon,
                            fields = to_recast,
                            cast = list(dropdown = castRaw,
                                        radio = castRaw,
                                        checkbox = castRaw,
                                        yesno = castRaw,
                                        truefalse = as.numeric))

    expect_equal(sort(unique(Recast$dropdown_test)),     c(1, 2, 3))
    expect_equal(sort(unique(Recast$radio_test)),        c("a", "b", "c"))
    expect_equal(sort(unique(Recast$checkbox_test___x)), c(0, 1))
    expect_equal(levels(Recast$checkbox_test___y),       levels(Records$checkbox_test___y))
    expect_equal(levels(Recast$checkbox_test___z),       levels(Records$checkbox_test___z))
    expect_equal(levels(Recast$yesno_test),              c("No", "Yes"))
    expect_true(is.logical(Recast$truefalse_test))

    to_recast <- logical(ncol(Records))
    to_recast[c(9, 6, 11)] <- TRUE
    Recast <- recastRecords(Records,
                            rcon = rcon,
                            fields = to_recast,
                            cast = list(dropdown = castRaw,
                                        radio = castRaw,
                                        checkbox = castRaw,
                                        yesno = castRaw,
                                        truefalse = as.numeric))

    expect_equal(levels(Recast$dropdown_test),           levels(Records$dropdown_test))
    expect_equal(sort(unique(Recast$radio_test)),        c("a", "b", "c"))
    expect_equal(levels(Recast$checkbox_test___x),       levels(Records$checkbox_test___x))
    expect_equal(sort(unique(Recast$checkbox_test___y)), c(0, 1))
    expect_equal(levels(Recast$checkbox_test___z),       levels(Records$checkbox_test___z))
    expect_equal(sort(unique(Recast$yesno_test)),        c(0, 1))
    expect_true(is.logical(Recast$truefalse_test))

    expect_error(recastRecords(Records,
                               rcon = rcon,
                               fields = rep(c(FALSE, TRUE),
                                            length.out = 7)),
                 "'fields' [(]logical[)] should be of length 11 and is length 7")
  }
)

test_that(
  "Suffixes applied correctly",
  {
    Recast <- recastRecords(Records,
                            rcon = rcon,
                            fields = names(Records),
                            suffix = "_coded",
                            cast = list(dropdown = castRaw,
                                        radio = castRaw,
                                        checkbox = castRaw,
                                        yesno = castRaw,
                                        truefalse = as.numeric))

    expect_equal(levels(Recast$dropdown_test),     levels(Records$dropdown_test))
    expect_equal(levels(Recast$radio_test),        levels(Recast$radio_test))
    expect_equal(levels(Recast$checkbox_test___x), levels(Records$checkbox_test___x))
    expect_equal(levels(Recast$checkbox_test___y), levels(Records$checkbox_test___x))
    expect_equal(levels(Recast$checkbox_test___z), levels(Records$checkbox_test___z))
    expect_equal(levels(Recast$yesno_test),        levels(Records$yesno_test))
    expect_true(is.logical(Recast$truefalse_test))

    expect_equal(sort(unique(Recast$dropdown_test_coded)),     c(1, 2, 3))
    expect_equal(sort(unique(Recast$radio_test_coded)),        c("a", "b", "c"))
    expect_equal(sort(unique(Recast$checkbox_test___x_coded)), 0:1)
    expect_equal(sort(unique(Recast$checkbox_test___y_coded)), 0:1)
    expect_equal(sort(unique(Recast$checkbox_test___z_coded)), 0:1)
    expect_equal(sort(unique(Recast$yesno_test_coded)),        0:1)
    expect_equal(sort(unique(Recast$truefalse_test_coded)),    0:1)
  }
)

test_that(
  "Handle mChoice fields by skipping them",
  {
    hmisc_installed <- require(Hmisc)

    skip_if(!hmisc_installed,
            "Hmisc is not installed")

    Records <-
      exportRecordsTyped(rcon,
                         fields = fields) |>
      mChoiceCast(rcon)

    Recast <- recastRecords(Records,
                            rcon = rcon,
                            fields = names(Records),
                            cast = list(dropdown = castRaw,
                                        radio = castRaw,
                                        checkbox = castRaw,
                                        yesno = castRaw,
                                        truefalse = as.numeric))

    expect_class(Recast$checkbox_test, "mChoice")
    expect_identical(Records$checkbox_test,
                     Recast$checkbox_test)

    detach("package:Hmisc", unload = TRUE)
  }
)

#####################################################################
# Argument Validation                                            ####

test_that(
  "Return an error if data is not a data.frame",
  {
    local_reproducible_output(width = 200)
    expect_error(recastRecords(data = "not data",
                               rcon = rcon,
                               fields = export_fields),
                 "Variable 'data': Must be of type 'data.frame'")
  }
)

test_that(
  "Return an error if rcon is not a redcapConnection object",
  {
    local_reproducible_output(width = 200)
    expect_error(recastRecords(data = Records,
                               rcon = "not a thing",
                               fields = export_fields),
                 "Variable 'rcon': Must inherit from class 'redcapConnection'")
  }
)

test_that(
  "Return an error if fields is not numeric, character, or logical",
  {
    local_reproducible_output(width = 200)
    expect_error(recastRecords(data = Records,
                               rcon = rcon,
                               fields = mtcars),
                 "'fields': One of the following must apply")
  }
)

test_that(
  "Return an error if recast is not a named list",
  {
    local_reproducible_output(width = 200)
    expect_error(recastRecords(data = Records,
                               rcon = rcon,
                               fields = export_fields,
                               cast = "this isn't a list"),
                 "Variable 'cast': Must be of type 'list'")

    expect_error(recastRecords(data = Records,
                               rcon = rcon,
                               fields = export_fields,
                               cast = list(castChecked)),
                 "Variable 'cast': Must have names.")
  }
)

test_that(
  "Return an error if suffix is not character(1)",
  {
    local_reproducible_output(width = 200)
    expect_error(recastRecords(data = Records,
                               rcon = rcon,
                               fields = export_fields,
                               suffix = 123),
                 "Variable 'suffix': Must be of type 'character'")

    expect_error(recastRecords(data = Records,
                               rcon = rcon,
                               fields = export_fields,
                               suffix = c("_label", ".label")),
                 "Variable 'suffix': Must have length 1")
  }
)

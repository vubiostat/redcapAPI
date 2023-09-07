context("Export Typed Reports Argument Validation")

#####################################################################
# exportReportsTyped                                             ####

test_that(
  "Return an error if rcon is not a redcapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportReportsTyped("not an rcon"), 
                 "no applicable method for 'exportReportsTyped'")
  }
)

test_that(
  "Return an error if report_id is not integerish(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportReportsTyped(rcon, 
                                    report_id = c(123, 456)), 
                 "'report_id': Must have length 1")
  }
)

test_that(
  "Return an error if drop_fields is not character",
  {
    local_reproducible_output(width = 200)
    expect_error(exportReportsTyped(rcon,
                                    report_id = 123,
                                    drop_fields = 1:2),
                 "'drop_fields'[:] Must be of type 'character'")
  }
)

test_that(
  "Return an error if na is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportReportsTyped(rcon,
                                    report_id = 123,
                                    na=1:3),
                 "'na'[:] Must be of type 'list'")
    expect_error(exportReportsTyped(rcon,
                                    report_id = 123,
                                    na=list("a")),
                 "'na'[:] Must have names")
  }
)

test_that(
  "Return an error if validation is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportReportsTyped(rcon,
                                    report_id = 123,
                                    validation=1:3),
                 "'validation'[:] Must be of type 'list'")
    expect_error(exportReportsTyped(rcon,
                                    report_id = 123,
                                    validation=list("a")),
                 "'validation'[:] Must have names")
  }
)

test_that(
  "Return an error if cast is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportReportsTyped(rcon,
                                    report_id = 123,
                                    cast=1:3),
                 "'cast'[:] Must be of type 'list'")
    expect_error(exportReportsTyped(rcon,
                                    report_id = 123,
                                    cast=list("a")),
                 "'cast'[:] Must have names")
  }
)

test_that(
  "Return an error if assignment is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportReportsTyped(rcon,
                                    report_id = 123,
                                    assignment=1:3),
                 "'assignment'[:] Must be of type 'list'")
    expect_error(exportReportsTyped(rcon,
                                    report_id = 123,
                                    assignment=list("a")),
                 "'assignment'[:] Must have names")
  }
)

test_that(
  "Return an error if config is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportReportsTyped(rcon,
                                    report_id = 123, 
                                    config=1:3),
                 "'config'[:] Must be of type 'list'")
    expect_error(exportReportsTyped(rcon,
                                    report_id = 123,
                                    config=list("a")),
                 "'config'[:] Must have names")
  }
)

test_that(
  "Return an error if api_param is not a list",
  {
    local_reproducible_output(width = 200)
    expect_error(exportReportsTyped(rcon,
                                    report_id = 123,
                                    api_param=1:3),
                 "'api_param'[:] Must be of type 'list'")
    expect_error(exportReportsTyped(rcon,
                                    report_id = 123,
                                    api_param=list("a")),
                 "'api_param'[:] Must have names")
  }
)

test_that(
  "Return an error if csv_delimiter is not an allowed character",
  {
    local_reproducible_output(width = 200)
    expect_error(exportReportsTyped(rcon,
                                    report_id = 123, 
                                    csv_delimiter="*"),
                 "'csv_delimiter'[:] Must be element of set")
    expect_error(exportReportsTyped(rcon,
                                    report_id = 123, 
                                    csv_delimiter=",,"),
                 "'csv_delimiter'[:] Must be element of set")
  }
)

context("isZeroCodedCheckField Functionality")

test_that(
  "test isZeroCodedCheckField",
  {
    expect_true(isZeroCodedCheckField("checkbox___0"))
    expect_true(isZeroCodedCheckField("some_field_name___0"))
    expect_false(isZeroCodedCheckField("four_underscore____0"))
    expect_false(isZeroCodedCheckField("checkbox___0_"))
    expect_false(isZeroCodedCheckField("checkbox___1"))
    expect_false(isZeroCodedCheckField("checkbox___a"))
    expect_false(isZeroCodedCheckField("checkbox___00"))
  }
)

test_that(
  "Return an error if field_name is not character(1)",
  {
    expect_error(isZeroCodedCheckField(0))
    expect_error(isZeroCodedCheckField(c("checkbox___0", 
                                         "checkbox___1")))
  }
)

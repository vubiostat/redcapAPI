context("attributeAssignment Functionality")


test_that(
  "stripHTMLandUnicode Functionality", 
  {
    expect_equal(
      stripHTMLandUnicode("field_name", "<p>text\U00B5</p>", "annotation"), 
      "text"
    )
  }
)


test_that(
  "unitsFieldAnnotation Functionality", 
  {
    expect_equal(
      unitsFieldAnnotation("field", "label", "units={\"meters\"}"), 
      "meters"
    )
  }
)

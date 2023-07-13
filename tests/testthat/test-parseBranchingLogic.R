context("parseBranchingLogic")


test_that(
  "Test that equality and inequality is parsed correctly", 
  {
    expect_identical(
      parseBranchingLogic(c("[value] = '5'",
                            "[value] < '5'", 
                            "[value] <= '5'", 
                            "[value] > '5'", 
                            "[value] >= '5'", 
                            "[value] <> '5'", 
                            "[value] != '5'")), 
      list(expression(value == "5"), 
           expression(value < "5"), 
           expression(value <= "5"), 
           expression(value > "5"), 
           expression(value >= "5"), 
           expression(value != "5"), 
           expression(value != "5"))
    )
  }
)

test_that(
  "Test that upper case checkbox labels are converted to lower case in the field name",
  {
    expect_identical(
      parseBranchingLogic("checkbox(UPPER) = '1'"), 
      list(expression(checkbox___upper == '1'))
    )
  }
)

test_that(
  "Test that upper/mixed case labels for radio boxes (or drop downs) are preserved in comparison", 
  {
    expect_identical(
      parseBranchingLogic(c("radio_UPPER = 'ABC'", 
                            "radio_lower = 'abc'", 
                            "radio_MIXed = 'Abc'")), 
      list(expression(radio_UPPER == 'ABC'),
           expression(radio_lower == 'abc'), 
           expression(radio_MIXed == 'Abc'))
    )
  }
)

test_that(
  "Test use of conjunctions AND/OR", 
  {
    logic <- c("[abc] < 1 AND [xyz] > 10", 
               "[abc] < 1 OR [xyz] > 10", 
               "[abc] < 1 and [xyz] > 10", 
               "[abc] < 1 or [xyz] > 10")
    
    expect_identical(parseBranchingLogic(logic), 
                     list(expression(abc < 1 & xyz > 10), 
                          expression(abc < 1 | xyz > 10), 
                          expression(abc < 1 & xyz > 10), 
                          expression(abc < 1 | xyz > 10)))
  }
)

context("Invalid Report Printing and Formatting")

Invalid <- structure(
  list(
    row = c(4L, 6L, 8L),
    record_id = c("2", "3", "4"),
    field_name = c("number_test", "number_test", "number_test"),
    form_name = c("numbers", "numbers", "numbers"),
    field_type = c("number", "number", "number"),
    event_id = c(507648L, 507648L, 507648L), 
    value = c("19.14555454", "14.37999015", "26.32266119"),
    link_to_form = rep("https://redcap.vumc.org/redcap_v14.3.12/DataEntry/index.php?pid=1234&page=numbers&id=2&event_id=1234", 3)
  ),
  time = "Tue 28 May 2024 12:00:00 AM UTC",
  version = "14.3.12",
  project = "TestRedcapAPI (gitlab)",
  row.names = c(NA, 3L),
  class = c("invalid", "data.frame")
)
    
test_that(
  "format invalid works", 
  {
    expect_character(format(Invalid), len=1, 
      pattern="Failed Validations.*3 failures.*Row 4, Record Id '2'.*Row 6, Record Id '3'.*\\[link\\]")
  }
)

test_that(
  "summary invalid works",
  {
    expect_character(summary(Invalid), len=1,
      pattern="3 validation failures.*number_test")
  }
)

test_that(
  "print invalid works",
  {
    output <- capture_output(print(Invalid))   
    expect_character(output, len=1, pattern="Failed Validations.*3 failures.*Row 4, Record Id '2'")
  }
)
  


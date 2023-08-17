context("guessCast.R")

x <- data.frame(
  x=c("xyz", "2023-01-01", "", "2003-12-12", "2003-12-12", "2012-10-10")
)

test_that(
  "guess cast works across multiple casts",
  {
    recs <- exportRecordsTyped(rcon, cast=raw_cast) |> 
      guessCast(rcon, 
                validation=valRx("^[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])$"), 
                cast=as.Date,
                threshold=0.3)
    expect_class(recs$prereq_date,   "Date")
    expect_class(recs$date_dmy_test, "Date")
    expect_class(recs$date_mdy_test, "Date")
    expect_class(recs$date_ymd_test, "Date")
    
  }
)

test_that(
  "guess cast preserves attributes",
  {
    recs <- exportRecordsTyped(rcon, cast=raw_cast) |>
      guessCast(rcon, 
                validation=valRx("^[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])$"), 
                cast=as.Date,
                threshold=0.3)
    
    expect_class(attr(recs$prereq_date, "label"),   "character")
  }
)

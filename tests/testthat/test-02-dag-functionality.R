# FIXME: Add tests after writing functions

# FIXME: exportDags was written with an immediate need. The tests provided
#        here should be replaced after importDags and deleteDags have 
#        been written.

test_that(
  "Returns a data frame of DAGs",
  {
    Dags <- exportDags(rcon)
    
    expect_data_frame(Dags, ncols = 3)
  }
)

context("allocationTable")

rcon <- redcapConnection(url, API_KEY)

test_that("allocation table can be generated",{
  expect_silent(
    allocationTable(rcon, 
                    random = "treatment", 
                    replicates = 8,
                    block.size = 8,
                    seed.dev = 10,
                    seed.prod = 20,
                    weights = c(Control = 1, Treatment = 1))
  )
})


test_that("allocation table can be updated offline",{
  
  filename <- tempfile()
  on.exit(unlink(filename))
  
  meta_data20 <- exportMetaData(rcon)
  write.csv(meta_data20, filename, na = "", row.names = FALSE)
  
  expect_silent(
    allocationTable_offline(filename,
                            random = "treatment", 
                            replicates = 8,
                            block.size = 8,
                            seed.dev = 10,
                            seed.prod = 20,
                            weights = c(Control = 1, Treatment = 1))
  )
})


#####################################################################
# Argument Validation

test_that(
  "Return an error when rcon is not a redcapConnection object", 
  {
    local_reproducible_output(width = 200)
    expect_error(allocationTable("not an rcon"), 
                 "no applicable method for 'allocationTable'")
  }
)


test_that(
  "Return an error when random is not character(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(allocationTable(rcon, 
                                 random = c("field1", "field2"), 
                                 replicates = 20, 
                                 block.size = 20), 
                 "Variable 'random': Must have length 1")
    expect_error(allocationTable(rcon, 
                                 random = TRUE, 
                                 replicates = 20, 
                                 block.size = 20), 
                 "Variable 'random': Must be of type 'character'")
    
    # random must be an existing field
    
    expect_error(allocationTable(rcon, 
                                 random = "not a field", 
                                 replicates = 20, 
                                 block.size = 20), 
                 "'random': Must be a subset of")
    
    # random is a categorical variable
    expect_error(allocationTable(rcon, 
                                 random = "no_prereq_number", 
                                 replicates = 20, 
                                 block.size = 20),
                 "'no_prereq_number' is not a valid variable for stratification/randomization")
  }
)

test_that(
  "Return an error when strata is not character",
  {
    local_reproducible_output(width = 200)
    expect_error(allocationTable(rcon, 
                                 random = "treatment", 
                                 replicates = 20, 
                                 block.size = 20, 
                                 strata = 1:3), 
                 "Variable 'strata': Must be of type 'character'")
    
    # Strata must be an existing field
    expect_error(allocationTable(rcon, 
                                 random = "treatment", 
                                 replicates = 20, 
                                 block.size = 20, 
                                 strata = "not a field"), 
                 "'strata': Must be a subset of")
    
    # Strata must be categorical
    expect_error(allocationTable(rcon, 
                                 random = "treatment", 
                                 replicates = 20, 
                                 block.size = 20, 
                                 strata = "no_prereq_number"), 
                 "'no_prereq_number' is not a valid variable for stratification/randomization")
  }
)

test_that(
  "Return an error when group is not character",
  {
    local_reproducible_output(width = 200)
    expect_error(allocationTable(rcon, 
                                 random = "treatment", 
                                 replicates = 20, 
                                 block.size = 20, 
                                 group = 1:3), 
                 "Variable 'group': Must be of type 'character'")
    
    # group must be an existing field
    expect_error(allocationTable(rcon, 
                                 random = "treatment", 
                                 replicates = 20, 
                                 block.size = 20, 
                                 group = "not a field"), 
                 "'group': Must be a subset of")
    
    # group must be categorical
    expect_error(allocationTable(rcon, 
                                 random = "treatment", 
                                 group = "no_prereq_number", 
                                 replicates = 20, 
                                 block.size = 20), 
                 "'no_prereq_number' is not a valid variable for stratification/randomization")
  }
)

test_that(
  "Return an error when dag.id is not integerish", 
  {
    expect_error(allocationTable(rcon, 
                                 random = "treatment", 
                                 group = "no_prereq_checkbox", 
                                 replicates = 20, 
                                 block.size = 20, 
                                 dag.id = "1"))
  }
)

test_that(
  "Return an error when replicates is not integerish(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(allocationTable(rcon, 
                                 random = "treatment", 
                                 block.size = 20, 
                                 replicates = 1:2), 
                 "Variable 'replicates': Must have length 1")
    
    expect_error(allocationTable(rcon, 
                                 random = "treatment", 
                                 block.size = 20, 
                                 replicates = "one"), 
                 "Variable 'replicates': Must be of type 'integerish'")
    
    expect_error(allocationTable(rcon, 
                                 random = "treatment", 
                                 block.size = 20, 
                                 replicates = pi), 
                 "Variable 'replicates': Must be of type 'integerish'")
  }
)

test_that(
  "Return an error when block.size is not integerish", 
  {
    local_reproducible_output(width = 200)
    expect_error(allocationTable(rcon, 
                                 random = "treatment", 
                                 replicates = 4, 
                                 block.size = "3"), 
                 " Variable 'block.size': Must be of type 'integerish'")
    
    expect_error(suppressWarnings(allocationTable(rcon, 
                                                  random = "treatment",
                                                  replicates = 4,
                                                  block.size = 3, 
                                                  seed.dev = 10, 
                                                  seed.prod = 13)), 
                 "'block.size' must be a multiple of 2")
  }
)

test_that(
  "Validate block.size.shift", 
  {
    local_reproducible_output(width = 200)
    expect_error(allocationTable(rcon, 
                                 random = "treatment", 
                                 replicates = 20, 
                                 block.size = c(8, 4, 2), 
                                 block.size.shift = c(1, 0.5, 0.9), 
                                 seed.dev = 10, 
                                 seed.prod = 11), 
                 "The first element of 'block.size.shift' must be 0")
    
    expect_error(allocationTable(rcon, 
                                 random = "treatment", 
                                 replicates = 20, 
                                 block.size = c(8, 4, 2), 
                                 block.size.shift = c(0, 0.7, 0.5), 
                                 seed.dev = 10, 
                                 seed.prod = 11), 
                 "'block.size.shift' must be strictly increasing on the interval")
    
    expect_error(allocationTable(rcon, 
                                 random = "treatment", 
                                 replicates = 20, 
                                 block.size = c(8, 4, 2), 
                                 block.size.shift = c(0, 0.5, 0.7, 0.9), 
                                 seed.dev = 10, 
                                 seed.prod = 11), 
                 "'block.size' and 'block.size.shift' must have the same length")
  }
)

test_that(
  "Validate seed.dev and seed.prod", 
  {
    local_reproducible_output(width = 200)
    expect_error(suppressWarnings(allocationTable(rcon, 
                                                  random = "treatment", 
                                                  replicates = 20,
                                                  block.size = 20,
                                                  seed.dev = c(10, 12), 
                                                  seed.prod = 11)), 
                 "'seed.dev' is a required argument and must be length")
    
    expect_error(suppressWarnings(allocationTable(rcon, 
                                                  random = "treatment", 
                                                  replicates = 20,
                                                  block.size = 20,
                                                  seed.dev = 10, 
                                                  seed.prod = 11:12)), 
                 "'seed.prod' is a required argument and must be length")
    
    expect_error(suppressWarnings(allocationTable(rcon, 
                                                  random = "treatment", 
                                                  replicates = 20, 
                                                  block.size = c(10, 10), 
                                                  block.size.shift = c(0, 0.5), 
                                                  seed.dev = c(10, 20), 
                                                  seed.prod = c(10, 15))), 
                 "No pairwise elements of 'seed.dev' and 'seed.prod' may be equal")
  }
)

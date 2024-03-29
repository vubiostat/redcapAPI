#' @name allocationTable
#' @title Generate Allocation Tables for the Randomization Module
#' 
#' @description These methods enable the user to generate allocation tables 
#' for the REDCap randomization module. Randomization may be stratified by other 
#' (categorical) variables in the data set as well as by Data 
#' Access Group. Additionally, randomization may be blocked 
#' to ensure balanced groups throughout the allocation
#' 
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param meta_data `character(1)`. For the offline method, a text string 
#'   giving the location of the data dictionary downloaded from REDCap.
#' @param random `character(1)`. The field name to be randomized.
#' @param strata `character`. Field names by which to stratify the randomization.
#' @param group `character(1)`. A field name giving a group by which randomization should be 
#'   stratified.  This could also be listed in `strata`, but the argument
#'   is provided to remain consistent with the REDCap user interface.
#' @param dag.id `integerish`. Data Access Group IDs.  
#' @param replicates `integerish(1)`. The number of randomizations to perform 
#'   within each stratum
#' @param block.size `integerish`. Block size for the randomization.  Blocking is recommended
#'   to ensure balanced groups throughout the randomization.  This may be a vector
#'   to indicate variable block sizes throughout the randomization.
#' @param block.size.shift `numeric` on the interval `[0, 1]`. 
#'   A vector the same length as `block.size` where the
#'   first element is 0.  This controls when the block size changes as a proportion
#'   of the total sample size.  When `block.size=c(8, 4, 2)` and 
#' `block.size.shift = c(0, .5, .9)`, the first half of the randomization 
#'   is performed in blocks of 8, then the next 40 percent of the randomization
#'   is performed in blocks of 4, with the last ten percent performed in blocks
#'   of 2.
#' @param seed.dev `integerish`. At least one value is required.  If only one value is given, 
#'   it will be converted to a vector with length equal to the number of strata.
#'   Values will be incremented by 100 to provide independent randomizations.
#'   This may also have length equal to the number of strata.
#' @param seed.prod `integerish`. Same as `seed.dev`, but used to seed the production
#'   allocation.  No pairwise elements of `seed.dev` and `seed.prod`
#'   may be equal.  This guarantees that the two randomization schemes are 
#'   unique.
#' @param weights An optional vector giving the sampling weights for each of the randomization 
#'   groups.  There must be one number for each level of the randomization variable.  If named, 
#'   the names must match the group labels.  If unnamed, the group labels will be assigned in the
#'   same order they appear in the data dictionary.  The weights will be normalized, so they do
#'   not need to sum to 1.0.  In other words, `weights=c(3, 1)` can indicate a 3:1 sampling
#'   ratio.
#' 
#' @details Each element in `block.size` must be a multiple of the number of 
#'   groups in the randomized variable.  
#' 
#'   The 'offline' version of the function operates on the data dictionary file 
#'   downloaded from REDCap.  This is made available for instances where the
#'   API cannot be accessed for some reason (such as waiting for API approval 
#'   from the REDCap administrator).
#' 
#'   The value of `replicates` controls how many allocations are generated.  It 
#'   is possible to get slightly more replicates than requested if your blocking design
#'   cannot exactly match replicates.  For example, if the users asks for 30 replicates in 
#'   blocks of 8, a warning will be printed and 32 replicates will be returned in the
#'   randomization table.
#'   
#' @return 
#' Returns a list with the elements
#' 
#' |                    |                                          |
#' |--------------------|------------------------------------------|
#' | `dev_allocation`   | `data.frame` with the randomization allocations for the development environment. |
#' | `prod_allocation`  | `data.frame` with the randomization allocations for the production environment. |
#' | `dev_seed`         | The random seed values for the development environment. |
#' | `prod_seed`        | The random seed values for the production environment. |
#' | `blocks`           | Blocking scheme used to generate the randomization. | 
#' | `weights`          | Weighting scheme for the randomization. | 
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' allocationTable(rcon, 
#'                 random = "treatment_assignment", 
#'                 strata = c("demographic_group", "hospital_group"), 
#'                 replicates = 12, 
#'                 block.size = 4, 
#'                 seed.dev = 12345, 
#'                 seed.prod = 54321)
#' }
#' 
#' @export

allocationTable <- function(rcon, 
                            random, 
                            strata = NULL, 
                            group = NULL, 
                            dag.id = NULL, 
                            replicates, 
                            block.size, 
                            block.size.shift = 0,
                            seed.dev = NULL, 
                            seed.prod = NULL,  
                            weights = NULL, 
                            ...){
  UseMethod("allocationTable")
}

#' @rdname allocationTable
#' @export

allocationTable.redcapApiConnection <- function(rcon, 
                                                random, 
                                                strata = NULL, 
                                                group = NULL, 
                                                dag.id = NULL, 
                                                replicates, 
                                                block.size, 
                                                block.size.shift = 0,
                                                seed.dev = NULL, 
                                                seed.prod = NULL,  
                                                weights = c(1, 1), 
                                                ...){
  
  coll <- checkmate::makeAssertCollection()
  
  #* Establish the meta_data table
  
  MetaData <- rcon$metadata()
  
  #* A utility function to extract the coded values from the meta_data
  redcapChoices <- function(v, meta_data, raw=TRUE)
  {
    if (meta_data$field_type[meta_data$field_name == v] %in% c("dropdown", "radio")){
      choice_str <- meta_data$select_choices_or_calculations[meta_data$field_name == v]
      choice_str <- fieldChoiceMapping(choice_str, v)
      return(choice_str[, (2-raw)])
    }
    else if (meta_data$field_type[meta_data$field_name == v] %in% c("yesno", "true_false"))
      return(0:1)
    else stop(paste0("'", v, "' is not a valid variable for stratification/randomization"))
  }
  
  #***************************************
  #* Parameter Checking
  #* These have gotten out of order to facilitate better error reporting
  #* 1. Verifying that 'random' is not missing
  #* 2. random, strata and group are characters
  #* 3. random and group have length 1
  #* 4. all fields in 'random', 'strata', and 'group' exist in meta_data 
  #* 5. Calculate n_strata
  #* 6. Verify 'replicates' is not missing and is numeric
  #* 7. If 'blocks.size' is missing, set it equal to 'replicates'.
  #*    If not missing, it must be numeric
  #* 9. block.size must be a multiple of n_strata
  #* 9. First element in block.size.shift must be 0
  #* 10. block.size.shift must be strictly increasing in the interval [0, 1)
  #* 11. block.size.shift must have the same length as block.size
  #* 12. The sum of all of the blocks must add up to replicates
  #* 13. Check if all blocks conform to blocking design (warning produced)
  #* 14. seed.dev is not NULL and has length 1 or n_strata
  #* 15. seed.prod is not NULL and has length 1 or n_strata
  #* 16. no pairwise elements of seed.dev are equal to seed.prod
  
  #* 1. Verifying that 'random' is not missing
  checkmate::assert_character(x = random,
                              len = 1,
                              add = coll)
  
  #* 2. random, strata and group are characters
  checkmate::assert_character(x = strata,
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_character(x = group,
                              len = 1,
                              null.ok = TRUE,
                              add = coll)
  
  #* 4. all fields in 'random', 'strata', and 'group' exist in meta_data
  #* Verify that all given fields exist in the database
  checkmate::assert_subset(x = random,
                           choices = MetaData$field_name,
                           add = coll)
  
  checkmate::assert_subset(x = strata,
                           choices = MetaData$field_name,
                           add = coll)
  
  checkmate::assert_subset(x = group,
                           choices = MetaData$field_name,
                           add = coll)
  
  checkmate::assert_integerish(x = dag.id,
                               null.ok = TRUE,
                               add = coll)
  
  checkmate::assert_integerish(x = replicates,
                               len = 1,
                               add = coll)
  
  #* 7. If 'block.size' is missing, set it equal to 'replicates'.
  if (missing(block.size)){
    block.size <- replicates
    warning("'block.size' was not provided.  The value of 'replicates' is used")
  }
  else{
    checkmate::assert_integerish(x = block.size,
                                 add = coll)
  }
  
  checkmate::reportAssertions(coll)

  
  #* 5. Calculate n_levels
  #* randomization levels
  random_levels <- redcapChoices(random, MetaData)
  random_level_names <- redcapChoices(random, MetaData, raw = FALSE)
  n_levels <- length(random_levels)
  
  #* stratification groups
  strata <- c(strata, group)
  strata_levels <- lapply(strata, redcapChoices, MetaData)
  names(strata_levels) <- strata
  if (!is.null(dag.id)) strata_levels[['redcap_data_access_group']] <- dag.id
  
  #* Allocation table
  allocation <- expand.grid(strata_levels)
  if (nrow(allocation) == 0) allocation <- data.frame(place.holding.strata=1)
  
  n_strata <- nrow(allocation)
  

  
  #* 8. block.size must be a multiple of n_levels
  if (any((block.size %% n_levels) != 0)){
    coll$push(paste0("'block.size' must be a multiple of ", n_levels))
  }
  
  #* 9. First element in block.size.shift must be 0
  if (block.size.shift[1] != 0){
    coll$push(": The first element of 'block.size.shift' must be 0")
  }
  
  #* 10. block.size.shift must be strictly increasing in the interval [0, 1)
  if (!all(block.size.shift >= 0) | !all(block.size.shift < 1) | 
      !all(diff(block.size.shift) > 0)){
    coll$push("'block.size.shift' must be strictly increasing on the interval [0, 1)")
  }
  
  #* 11. block.size.shift must have the same length as block.size
  if (length(block.size) != length(block.size.shift)){
    coll$push(": 'block.size' and 'block.size.shift' must have the same length")
  }
  
  checkmate::reportAssertions(coll)
  
  #* 12. The sum of all of the blocks must add up to replicates
  max.n <- cumsum(diff(c(block.size.shift * replicates, replicates)))
  
  blocks <- NULL
  for (i in 1:length(block.size)){
    while(sum(blocks) < max.n[i]){
      blocks <- c(blocks, block.size[i])
    }
  }
  
  Blocks <- data.frame(block.num = 1:length(blocks),
                       block.size = blocks,
                       cum.n = cumsum(blocks))
  Blocks <- merge(Blocks, data.frame(block.size=block.size,
                                     max.n = max.n),
                  by="block.size", sort=FALSE)
  Blocks$conform <- with(Blocks, cum.n <= max.n)
  
  if (sum(Blocks$block.size) != replicates){
    warning("The sum of the block sizes should add up to 'replicates'\n",
            "  Please review the Blocks attribute and consider changing your blocking scheme")
  }
  
  #* 13. Check if all blocks conform to blocking design (warning produced)
  if (!all(Blocks$conform)){
    warning("The blocking design did not conform exactly to specifications\n",
             "  Please review the Blocks attribute and consider changing your blocking scheme")
  }
  
  #* 14. seed.dev is not NULL and has length 1 or n_strata
  if (ifelse(is.null(seed.dev), TRUE, !length(seed.dev) %in% c(1, n_strata))){
    coll$push(paste0("'seed.dev' is a required argument and must be length 1 or ", n_strata))
  }
  
  #* 15. seed.prod is not NULL and has length 1 or n_strata
  if (ifelse(is.null(seed.prod), TRUE, !length(seed.prod) %in% c(1, n_strata))){
    coll$push(paste0("'seed.prod' is a required argument and must be length 1 or ", n_strata))
  }
  
  #* 16. no pairwise elements of seed.dev are equal to seed.prod
  if (any(seed.dev == seed.prod)){
    coll$push("No pairwise elements of 'seed.dev' and 'seed.prod' may be equal")
  }
  
  #* 17. If 'weights' is not NULL, it is the same length as the number of levels in 'random'
  if (is.null(weights)){
    weights <- rep(1, length(random_levels))
    names(weights) <- random_levels
    warning("No 'weights' were given.  Equal weights have been assumed.")
  }
  
  #* 18. If 'weights' has names, the names are identical to the levels of 'random'
  if (!is.null(names(weights))){
    if (!identical(names(weights), random_level_names)) {
      coll$push(paste0("'weight' names must be '",
                       paste0(random_level_names, collapse = "', '"), 
                       "'."))
    }
  }
  #* 19. if 'weights' does not have names, assume the weights were given in the order of levels(random)
  else {
    names(weights) <- random_level_names
    warning(paste0("No names given with 'weights'.  The names '",
                   paste0(random_level_names, collapse = "', '"), 
                   "' have been assumed"))
  }
  
  weights_orig <- weights
  weights <- weights[random_level_names] / sum(weights)
  
  checkmate::reportAssertions(coll)
  
  if (length(seed.dev) == 1) seed.dev <- seed.dev + ((1:n_strata)-1)*100
  if (length(seed.prod) == 1) seed.prod <- seed.prod + ((1:n_strata)-1)*100
  
  if (is.null(weights)) weights <- rep(1, length(random_levels))
  weights <- weights / sum(weights)
  
  #* Randomization function
  Randomization <- function(choices, Blocks, seed, weights){
    set.seed(seed) #* set the seed
    #* Randomizations
    do.call("c", 
            lapply(X = Blocks$block.size, 
                   FUN = function(x) sample(makeChoices(choices, x, weights), 
                                            size = x)))
  }
  
  #   return(list(allocation, Blocks, random_levels, seed.dev))
  #* Generate an allocation table for each stratum (Development)
  dev_allocate <- 
    lapply(X = 1:nrow(allocation),
           FUN = function(r){
             a <- allocation[r, , drop=FALSE]
             #* extend the length of the stratum data frame to accomodate the sampling
             a <-  a[rep(row.names(a), sum(Blocks$block.size)), , drop=FALSE]
             a[[random]] <- Randomization(choices = random_levels, 
                                          Blocks = Blocks, 
                                          seed = seed.dev[r], 
                                          weights = weights)
             return(a)
           })
  
  #* Combine the allocation tables
  dev_allocate <- do.call("rbind", dev_allocate)
  
  #* reorder the allocation table for uploading to REDCap
  dev_allocate <- dev_allocate[, c(random, names(strata_levels)), drop=FALSE]
  rownames(dev_allocate) <- NULL  
  
  
  #* Generate an allocation table for each stratum (Production)
  prod_allocate <- 
    lapply(X = 1:nrow(allocation),
           FUN = function(r){
             a <- allocation[r, , drop=FALSE]
             #* extend the length of the stratum data frame to accomodate the sampling
             a <-  a[rep(row.names(a), sum(Blocks$block.size)), , drop=FALSE]
             a[[random]] <- Randomization(choices = random_levels, 
                                          Blocks = Blocks, 
                                          seed = seed.prod[r], 
                                          weights = weights)
             return(a)
           })
  #* Combine the allocation tables
  prod_allocate <- do.call("rbind", prod_allocate)
  
  #* reorder the allocation table for uploading to REDCap
  prod_allocate <- prod_allocate[c(random, names(strata_levels))]
  rownames(prod_allocate) <- NULL  
  
  
  return(list(dev_allocation = dev_allocate, 
              dev_seed = seed.dev,
              prod_allocation = prod_allocate, 
              prod_seed = seed.prod,
              blocks = Blocks,
              weights = weights_orig))
}

# @rdname allocationTable
# @param random_levels A vector of the randomization group level names.  Determined from the
#   data dictionary.

makeChoices <- function(random_levels, block.size, weights){
  group.size <- block.size * weights
  if (sum(ceiling(group.size) - group.size) == 0)
    choices <- rep(random_levels, 
                   times = group.size)
  else 
    choices <- sample(random_levels, 
                      size = block.size, 
                      replace = TRUE, 
                      prob = weights)
  choices
}

#' @rdname allocationTable
#' @export

allocationTable_offline <- function(meta_data, 
                                    random, 
                                    strata = NULL, 
                                    group = NULL, 
                                    dag.id = NULL, 
                                    replicates, 
                                    block.size, 
                                    block.size.shift = 0,
                                    seed.dev = NULL, 
                                    seed.prod = NULL,  
                                    weights = c(1, 1), 
                                    ...){
  MetaData <- utils::read.csv(meta_data,
                               stringsAsFactors=FALSE,
                               na.strings = "")
  
  col.names=c('field_name', 'form_name', 'section_header', 
              'field_type', 'field_label', 'select_choices_or_calculations', 
              'field_note', 'text_validation_type_or_show_slider_number', 
              'text_validation_min', 'text_validation_max', 'identifier', 
              'branching_logic', 'required_field', 'custom_alignment', 
              'question_number', 'matrix_group_name', 'matrix_ranking',
              'field_annotation')
  names(MetaData) <- col.names[1:length(col.names)]
  
  # Bear with me here--this is weird. If we can trick the redcapApiConnection
  # method into thinking the object it is passed has a cached metadata frame, 
  # we can just pass everything up to that method for processing. So we're
  # going to spoof a redcapApiConnection object here. It's a dirty trick.
  # Try not to do it elsewhere. I'm seriously questioning my sanity for 
  # doing it here.
  
  spoofedConnection <- function(metadata){
    md <- metadata
    rc <- list(metadata = function() md)
    class(rc) <- "redcapApiConnection"
    rc
  }
  
  allocationTable.redcapApiConnection(rcon = spoofedConnection(MetaData),
                                      random = random,
                                      strata = strata,
                                      group = group,
                                      dag.id = dag.id,
                                      replicates = replicates,
                                      block.size = block.size,
                                      block.size.shift = block.size.shift,
                                      seed.dev = seed.dev,
                                      seed.prod = seed.prod,
                                      weights = weights, 
                                      ...)
}

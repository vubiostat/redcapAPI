#' @name massert
#' @title Conduct Multiple Assertions
#' 
#' @description This documentation attempts to describe arguments to make assertions
#' on arguments.  In order to prevent confusion, it is imperative to develop some
#' terminology up front. We will use _function argument_ to refer to an argument
#' of the function for which we are conducting assertions.  We will  use 
#' _assertion argument_ to refer to arguments to pass to the assertion function 
#' being applied to a function argument.  Lastly, we will use _massert argument_
#' to refer to arguments to `massert`
#'   
#' @param formula A one sided formula naming the arguments on which the assertion
#'   will be performed.
#' @param fun An assertion function to perform.
#' @param ... Additional lists. Each argument provided is a named list of `assertion 
#'   arguments`. The name of each element in a list should match the name of a 
#'   `function argument`. `lower = list(var1 = 0, var2 = 10)` sets 
#'   the _assertion argument_ `lower = 0` for _function argument_ 
#'   `var1`; and sets the _assertion argument_ `lower = 10` for 
#'   `function argument` `var2`. The _massert arguments_ in `...` 
#'   may themselves be named or unnamed.
#' @param fixed A named list of arguments that are fixed across all assertions.
#' 
#' @details Only one assert function may be utilized in each call to `massert`. 
#'   This allows for all numeric variables to be checked in one call, all logical 
#'   variables to be checked in a subsequent call, etc.
#'   

massert <- function(formula, fun, ..., fixed = list())
{
  checkmate::assert_class(x = formula,
                          classes = "formula")
  
  massert_coll <- checkmate::makeAssertCollection()
  
  # `fm` has no left hand side
  lhs <- all.vars(stats::update(formula, . ~ 0))
  
  if (!all(lhs == ".")) 
  {
    massert_coll$push("`formula` may not have a left hand side.")
  }
  
  checkmate::assert_function(x = fun,
                             add = massert_coll)
  
  checkmate::assert_list(x = fixed,
                         names = "named",
                         add = massert_coll)
  
  checkmate::reportAssertions(massert_coll)
  
  fun <- match.fun(fun)
  terms <- terms(formula)
  vnames <- attr(terms, "term.labels")
  ee <- attr(terms, ".Environment")
  
  unfixed <- list(...)
  
  unfixed_name <- unique(unlist(lapply(unfixed, names)))
  
  checkmate::assert_subset(x = unfixed_name,
                           choices = vnames)
  
  for (vname in vnames){
    # Get list elements from ... that match vname
    # These are the assertion arguments.
    this_var_arg <- lapply(unfixed,
                           function(x, n) x[[n]] ,
                           vname)
    # Remove NULL values
    this_var_arg <- this_var_arg[!vapply(this_var_arg, is.null, logical(1))]
    
    # Make the argument list
    args <- c(list(x = get(vname, envir = ee),
                   .var.name = vname),
              this_var_arg,
              fixed)
    
    # Perform the assertion
    do.call(fun, args)
  }
  invisible(NULL)
}

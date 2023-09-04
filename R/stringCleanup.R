#' @name stringCleanup
#' @title Remove Undesired Characters From Strings
#' 
#' @description These functions are utilities to clear undesired characters
#' from REDCap output.
#' 
#' @param x `character`, vector of content to be cleaned.
#' @param tags `character`, vector of HTML tags to remove from `x`
#' @param ignore.case `logical(1)`, should cases be ignored when matching
#'   patterns? Defaults to `TRUE`.
#'   
#' @return 
#' `stripHTMLTags` returns a `character` vector.
#' 
#' `stripUnicode` returns a `character` vector.
#' 
#' @examples
#' stripHTMLTags("<p>Text in a paragraph <b>tag</b> with bold formatting </p>")
#' 
#' stripUnicode("\U00B5 = 0")
#' 
#' @export

stripHTMLTags <- function(x, 
                          tags = c("p", "br", "div", "span", "b", "font", "sup", "sub"), 
                          ignore.case = TRUE){
  ###################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = x, 
                              add = coll)
  
  checkmate::assert_character(x = tags, 
                              add = coll)
  
  checkmate::assert_logical(x = ignore.case, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Regex explanation 
  # < : match the opening of the tag
  # ([/]|) : The pipe (|) is an or operator. So this is a match of either nothing or /
  #          This makes sure that both <p> and </p> tags are matched, for example
  # (%s) : filled with the tags argument, but collapsed to (p|br|...). This is
  #        matching the tags listed in the tags argument
  # (|.+) : matches either nothing following the tag identifier, or any number of characters
  #         until it reaches the closing >
  # *? : Make the match 'non-greedy', that is, it will start the search at < and stop
  #      at the first > it encounters.
  regex <- sprintf("<([/]|)(%s)(|.+)*?>", 
                   paste0(tags, 
                          collapse = "|"))
  x <- trimws(gsub(regex, "", x, ignore.case = ignore.case))
  x <- gsub("\\n", "", x)
  x
}

#' @rdname stringCleanup
#' @export

stripUnicode <- function(x)
{
  ###################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = x, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Regex explanation
  # Note: this is no longer the case for R on linux.
  # NEED: testing on Windows 10 / Windows 11 / Mac
  # See: https://stackoverflow.com/questions/39993715/how-to-remove-unicode-u00a6-from-string
  # <U\\+ - a literal char sequence <U+
  # \\w+ - 1 or more letters, digits or underscores
  # > - a literal >
  
  gsub("[^\x01-\x7F]+", "", x, perl=TRUE)
}

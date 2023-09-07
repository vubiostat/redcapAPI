context("Strip HTML And Unicode Functionality")

#####################################################################
# stripHTMLTags                                                  ####

test_that(
  "stripHTMLTags Argument Validation", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(stripHTMLTags(x = 123), 
                 "'x': Must be of type 'character'")
    
    expect_error(stripHTMLTags(x = "<p>text</p>", 
                               tags = 123), 
                 "'tags': Must be of type 'character'")
    
    expect_error(stripHTMLTags(x = "<p>text</p>", 
                               ignore.case = c(TRUE, FALSE)), 
                 "'ignore.case': Must have length 1")
    
    expect_error(stripHTMLTags(x = "<p>text</p>", 
                               ignore.case = "TRUE"), 
                 "'ignore.case': Must be of type 'logical'")
  }
)

test_that(
  "stripHTMLTags Functionality", 
  {
    tags = c("p", "br", "div", "span", "b", "font", "sup", "sub")
    
    test_case <- sprintf("<%s>text</%s>", tags, tags)
    expect_equal(stripHTMLTags(test_case), 
                 rep("text", length(tags)))
    
    test_case <- sprintf("<%s>text</%s>", tags, toupper(tags))
    expect_equal(stripHTMLTags(test_case), 
                 rep("text", length(tags)))
    
    
    expect_equal(stripHTMLTags("<p>text</P>", 
                               ignore.case = FALSE), 
                 c("text</P>"))
    
    expect_equal(stripHTMLTags("<not-a-tag>text</not-a-tag>", 
                               tags = "not-a-tag"), 
                 "text")
    
  }
)


#####################################################################
# stripUnicode                                                   ####

test_that(
  "stripUnicode Argument Validation", 
  {
    local_reproducible_output(width = 200)
    
    expect_error(stripUnicode(123), 
                 "'x': Must be of type 'character'")
  }
)

test_that(
  "stripUnicode Functionality", 
  {
    expect_equal(stripUnicode("\U00B5 = 0"), 
                 " = 0")
  }
)

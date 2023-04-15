#' @name constants
#' @title Constants for Use in the redcapAPI Package
#'
#' @description Constants that are used inside the package
#'

# UNEXPORTED --------------------------------------------------------



# This is used for File Repository methods. Some calls result in a
# result of an empty string or an error. At times we prefer to return
# the empty frame to maintain consistency in outputs with recursive calls
FILE_REPOSITORY_EMPTY_FRAME <- 
  data.frame(folder_id = numeric(0), 
             doc_id = numeric(0), 
             name = character(0), 
             parent_folder = numeric(0),
             stringsAsFactors = FALSE)

# This is used for cases when an import/export/delete to the file repository
# results in no changes and an empty frame is needed for the return

FILE_IMPORT_EXPORT_EMPTY_FRAME <- 
  data.frame(directory = character(0), 
             filename = character(0), 
             stringsAsFactors = FALSE)



# Regular Expressions -----------------------------------------------

# REGEX_CHECKBOX_FIELD_NAME - Matches the checkbox style field name
# use sub(REGEX_CHECKBOX_FIELD_NAME, "\\1", x) to get the field name base
# use sub(REGEX_CHECKBOX_FIELD_NAME, "\\2", x) to get the option
# Using regex to parse checkbox names has some limitations and at some point
# we may need to consider another strategy. REDCap permits any characters to
# be used in coding checkboxes, but when converting them to field names, 
# converts non alphnumeric characters to an underscore. 
# some examples -4, Option  ----> checkbox____4
#               k>=9, K >=9 ----> checkbox___k__9
#               a___b       ----> checkbox___a___b (this is not handled correctly by this REGEX)
# I don't think there is any way to truly parse the field name base from the 
# option without using the data dictionary.
# 
# Explanation : https://stackoverflow.com/a/76020417/1017276
# ^(.*?)___ : match anything at beginning non-greedily followed by ___ into 1st group
# (?!.*___) : after that, don't allow ___ preceded by anything, negative lookahead is used for that purpose
#     (.*)$ : match anything after until end of string into 2nd group
REGEX_CHECKBOX_FIELD_NAME <- "^(.*?)___(?!.*___)(.*)$"

# REGEX_FIELD_NAME - matches the acceptable naming conventions for field names
# Explanation
#            ^ : start of string
#        [a-z] : a single character that is a lower case letter
# [a-z,0-9,_]+ : any number or characters that are a lower case letter, digit, or underscore
#    [a-z,0-9] : a single character that is a lower case letter or digit
#            $ : end of string
REGEX_FIELD_NAME <- "^[a-z][a-z,0-9,_]+[a-z,0-9]$"

# REGEX_FORM_NAME - matches acceptable naming conventions for form names
# Explanation
#            ^ : start of string
#        [a-z] : A single character that is a lower case letter
# [a-z,0-9,_]+ : Any number of characters that are lower case letters, digits, or an underscore
#    [a-z,0-9] : a single character that is a lower case letter or digit
#            $ : end of string
REGEX_FORM_NAME <- "^[a-z][a-z,0-9,_]+[a-z,0-9]$"

# REGEX_MULT_CHOICE - matches acceptable formats for multiple choice options
# It's a good idea to trim whitespace before using this
# Explanation - this one makes my head swim a bit, but I'll do my best. (BN)
#                       ^ : Start of string
#                 [^\\|]+ : Any number of character, but the sequence may not 
#                           start with a pipe (this is the first code)
#                       , : literal comma
#                 [^\\|]* : any number of characters, but the sequence may not 
#                           start with a pipe (this is the first label)
#                           it is a lazy match, meaning it will stop the first 
#                           time it hits a pipe
# (?:\\|[^\\|]+,[^\\|]*)* : this is the workhorse, it's looking for a repeating pattern
#                         : of pipe, characters, comma, character, pipe with 
#                         : a terminating sequence of pipe, characters, comma, characters.
#                         : That is, the last in the sequence does not end with a pipe
#                       $ : end of string
REGEX_MULT_CHOICE <- "^[^\\|]+,[^\\|]*(?:\\|[^\\|]+,[^\\|]*)*$"

# REGEX_SLIDER - matches acceptable definition of slider bar settings
# Specifically, low point | midpoint | high point
# Any of the three values may be missing, but the two pipes must be 
# present.
# This is fairly permissive, but it seems to match REDCap behaviors
# REDCap doesn't actually restrict what characters can go between the pipes, 
# although it produces weird results when you include a pipe within the labels.
# Example:  this | and|or | that will produce labels "this", "and", and "or | that" in the UI.
# Explanation
#             ^ : start of string
# ([0-9]|\\s|)+ : any number of characters, or empty character
#           [|] : match a literal pipe
# ([0-9]|\\s|)+ : any number of characters, or empty character
#           [|] : match a literal pipe
# ([0-9]|\\s|)+ : any number of characters, or empty character
#             $ : end of string
REGEX_SLIDER <- "^(.|)+[|](.|\\s|)+[|](.|\\s|)+$"


# Constants that are used inside the package
#

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
# use sub(REGEX_CHECKBOX_FIELD_NAME, "\\1", x, perl = TRUE) to get the field name base
# use sub(REGEX_CHECKBOX_FIELD_NAME, "\\2", x, perl = TRUE) to get the option
# Using regex to parse checkbox names has some limitations and at some point
# we may need to consider another strategy. REDCap permits any characters to
# be used in coding checkboxes, but when converting them to field names, 
# converts non alphnumeric characters to an underscore. 
# some examples -4, Option  ----> checkbox____4
#               k>=9, K >=9 ----> checkbox___k__9
#               a___b       ----> checkbox___a___b
# 
# Explanation : https://stackoverflow.com/a/76020417/1017276
# ^(.*?)___ : match anything at beginning non-greedily followed by ___ into 1st group
#       ___ : after that, find the first case of ___ (the REDCap UI will not permit the user to use more than one consecutive _ in field names)
#     (.*)$ : match anything after until end of string into 2nd group
REGEX_CHECKBOX_FIELD_NAME <- "^(.*?)___(.*)$"

# REGEX_FIELD_NAME - matches the acceptable naming conventions for field names
# Explanation
# This regex has three parts
# 1. ^[a-z]$   : allows a field name of a single alpha character
# 2. ^[a-z][a-z,0-9]$ : allows a field name of two characters, an alpha followed by an alpha/numeric
# 3. ^[a-z](?!.*__.*)[a-z,0-9,_]+[a-z,0-9]$: 3 or more characters, broken down as follows
#            ^ : start of string
#        [a-z] : a single character that is a lower case letter
#   (?!.*__.*) : disallow any two consecutive characters to be underscores
# [a-z,0-9,_]+ : any number or characters that are a lower case letter, digit, or underscore
#    [a-z,0-9] : a single character that is a lower case letter or digit
#            $ : end of string
REGEX_FIELD_NAME <- "(^[a-z]$|^[a-z][a-z,0-9]$|^[a-z](?!.*__.*)[a-z,0-9,_]+[a-z,0-9]$)"

# REGEX_FORM_NAME - matches acceptable naming conventions for form names
# Explanation
# This regex has three parts
# 1. ^[a-z]$   : allows a field name of a single alpha character
# 2. ^[a-z][a-z,0-9]$ : allows a field name of two characters, an alpha followed by an alpha/numeric
# 3. ^[a-z](?!.*__.*)[a-z,0-9,_]+[a-z,0-9]$: 3 or more characters, broken down as follows
#            ^ : start of string
#        [a-z] : A single character that is a lower case letter
#   (?!.*__.*) : disallow any two consecutive characters to be underscores
# [a-z,0-9,_]+ : Any number of characters that are lower case letters, digits, or an underscore
#    [a-z,0-9] : a single character that is a lower case letter or digit
#            $ : end of string
REGEX_FORM_NAME <- "(^[a-z]$|^[a-z][a-z,0-9]$|^[a-z](?!.*__.*)[a-z,0-9,_]+[a-z,0-9]$)"

# REGEX_MULT_CHOICE_STRICT - matches acceptable formats for multiple choice options
# It's a good idea to trim whitespace before using this
# This 'STRICT' regex requires that definitions follow the pattern of 
# [code1], [label1] | [code2], [label2]. It turns out that there are ways 
# that users can define multiple choice fields using only the labels 
# (via the metadata CSV import and the API will actually allow it, too). 
# We have decided that we do not want to permit this through importMetaData
# so a separate regex is needed for the import than we use for the export.
# See issue 145.
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

REGEX_MULT_CHOICE_STRICT <- "^[^\\|]+,[^\\|]*(?:\\|[^\\|]+,[^\\|]*)*$"

# REGEX_MULT_CHOICE - matches acceptable formats for multiple choice options, 
# to include formats that use only the label. See Issue 145. 
# It's a good idea to trim whitespace before using this. 
# Explanation - uses the same pattern as REGEX_MULT_CHOICE_STRICT and also includes the following
#               for matching shorthand multiple choice fields.
#                       ^ : Start of string
#           (?:[^|,]+\|)+ : Look for a repeating pattern of character, pipe, character, where
#                           the last in the sequence does not end with a pipe and characters 
#                           do not include commas
#                  [^|,]+ : any number of characters, but the sequence may not 
#                           include a pipe or comma
#                       $ : end of string

REGEX_MULT_CHOICE <- "^(^[^\\|]+,[^\\|]*(?:\\|[^\\|]+,[^\\|]*)*$|^(?:[^|,]+\\|)+[^|,]+$)$"
                     
# REGEX_SLIDER - matches acceptable definition of slider bar settings
# Specifically, low point | midpoint | high point
# Any of the three values may be missing, but the two pipes must be 
# present.
# This is fairly permissive, but it seems to match REDCap behaviors
# REDCap does not actually restrict what characters can go between the pipes, 
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

# REGEX_POSIXCT - matches the pattern of a POSIXct value converted to character
# See: https://www.regular-expressions.info/dates.html#:~:text=To%20match%20a%20date%20in,%5D)%5B%2D%20%2F.%5D
REGEX_POSIXCT <- "^\\d{4}[-](0[1-9]|1[012])[-](0[1-9]|[12][0-9]|3[01]) ([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"

# REGEX_HHMMSS - matches the pattern of time in HH:MM:SS format
REGEX_HHMMSS <- "^([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"

# Regular Expressions for Date, DateTime, and Time formats

REGEX_DATE <- "^[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])$"
REGEX_DATETIME <- "^[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])\\s([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]$"
REGEX_DATETIME_SECONDS <- "^[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])\\s([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"
REGEX_TIME_MMSS <- "^[0-5][0-9]:[0-5][0-9]$"
REGEX_TIME_HHMMSS <- "^([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"
REGEX_TIME <- "^([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]$"
REGEX_LETTERS_ONLY <- "^(?i)[a-z]+$"
REGEX_FLOAT <- "^[-+]?(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))([Ee][+-]?[0-9]+)?$"
REGEX_NUMBER <- "^[-+]?(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))([Ee][+-]?[0-9]+)?$"
REGEX_NUMBER_COMMA <- "^[-+]?(([0-9]+,?[0-9]*)|(,[0-9]+))([Ee][+-]?[0-9]+)?$"
REGEX_CALC <- "^[-+]?(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))([Ee][+-]?[0-9]+)?$"
REGEX_INT <- "^[-+]?[0-9]+(|\\.|\\.[0]+)$"
REGEX_INTEGER <- "^[-+]?[0-9]+$"
REGEX_YES_NO <- "^(?i)(0|1|yes|no)$"
REGEX_TRUE_FALSE <- "^(?i)(0|1|true|false)$"
REGEX_CHECKBOX <- "^(?i)(0|1|yes|no|checked|unchecked)$"
REGEX_FORM_COMPLETE <- "^(?i)([012]|complete|incomplete)$"
REGEX_EMAIL <- "^(?i)[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}$"
REGEX_PHONE <- "^[2-9][0-8][0-9][2-9][0-9]{6}$" # Remove punctuation before using
REGEX_ZIPCODE <- "^(\\d{5}|\\d{5}-\\d{4})$"

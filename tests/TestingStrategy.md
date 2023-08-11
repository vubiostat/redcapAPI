# `redcapAPI` Testing Strategies

## Folder Structure

* tests
    + testthat  
        + testdata  
            + offlineConnectionFiles  
                + CSV Files for offlineConnection  
            + Files of Project Data for use in testing  
        + Testing Files  
    + TestingStrategy.md  
    + testthat.R  

## REDCap Instance Specific Constants

### Expendable User

In order to test the import/export/delete User methods, it will be necessary to 
have a username for testing. The difficulty here is that we cannot know who 
the current user is (tokens are specific to a user and this is tracked on the
server). Furthermore, if we happen to use the user that is running the tests, 
the user's access to the project could be cut off if the user is deleted, 
resulting in all subsequent tests failing. 

It is recommended that an _expendable user_ be created on the REDCap instance
that can be used in the testing of the User methods. You will need to work 
with your REDCap Administrator to set up this user. 

Once a user is set up, the user name should be entered in 
tests/testthat/helper-REDCapQACredentials.R as the `EXPENDABLE_USER` constant.

### Report ID for exportReports Methods

The API doesn't provide a way for us to retrieve the Report IDs of reports in 
the project. A report will need to be created for testing. The tests are 
designed to not require any specific fields, but we recommend including a variety
of numeric, date, and multiple choice fields to assist in testing.

Note: we can get away without extensive testing in the `exportReports` and
`exportReportsTyped` methods because they data processing use the same methods
implements in the `exportRecords` equivalents.

Once the report is created, it should be entered as the constant `EXPORT_REPORTS_ID`.

## Validating Arguments and Testing Functionality

There are many functions in `redcapAPI` that act as a system.  These usually
include an export, import, and delete method, such as `exportUsers`, 
`importUsers`, and `deleteUsers`. Rather than testing these independently, 
we are able to utilize them together to perform the tests and perform 
cleanup that restore the project to it's pre-test state. 

In order to avoid excessively long files and distracting argument validation 
testing, when a system involves multiple methods, we recommend performing
the argument validation in a separate file. Files where argument validation
and functionality are separated should use the same number. 


## File Organization

Organization of the testing files will use a numeric ordering system. The 
numeric ordering allows us to test the features in an order that ensures
infrastructural components and support functions are tested first, prior 
the to the methods that depend on them. 

The numbering outlined here allows for expansion of the package and its 
features.

**Establishing the connection.**

* helper-00-REDCapQACredentials.R
    + REDCap Instance Specific Constants

**Set up the File Repository.**

* helper-01-setupFileRepository.R

**Package Loading**

Files 001 - 019


**Support Functions**

Files 020 - 099


**Project Infrastructure**

Files 100 - 149


**Basic Records Import / Export / Deletion**

Files 150 - 199


**Typed Records Export**

Files 200 - 249


**Record Post Processing**

Files 250 - 299


**File Exports and Imports**

Files 300 - 349


**Other Methods**

Files 350 - 399



**Survey Methods**

Files 400 - 449


**Logging**

Files 800 - 810

**Deprecated**

Files 900 - 999






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


## Current Files For Development


**Typed Records Export**

Files 200 - 249

* test-240-typedReports-ArgumentValidation.R
* test-240-typedReports-Functionality.R

**Record Post Processing**

Files 250 - 299

* test-250-recastRecords.R
* test-251-castForImport.R
* test-252-guessCast.R
* test-253-guessDate.R
* test-254-mChoiceCast.R
* test-255-splitForms.R
* test-256-dropRepeatingNA.R

**File Exports and Imports**

Files 300 - 349

* test-300-reconstituteFileFromExport.R
* test-301-fileExportMethods-ArgumentValidation.R
* test-301-fileExportMethods-Functionality.R
* test-302-createFileRepository-ArgumentValidation.R
* test-302-createFileRespoitory-Functionality.R
    + Will be an empty file as there is no way to clean up afterward.
* test-303-fileRepository-SingleFileMethods-ArgumentValidation.R
* test-303-fileRepository-SingleFileMethods-Functionality.R
* test-304-fileRepository-BulkFileMethods-ArgumentValidation.R
* test-304-fileRepository-BulkFileMethods-Functionality.R

**Other Methods**

Files 350 - 399

* test-350-exportNextRecordName-ArgumentValidation.R
* test-350-exportNextRecordName-Functionality.R
* test-351-exportVersion.R
* test-352-savePurgeRestoreProject-ArgumentValidation.R
* test-352-savePurgeRestoreProject-Functionality.R
* test-353-allocationTable-ArgumentValidation.R
* test-353-allocationTable-Functionality.R
* test-354-getProjectIdFields.R
* test-355-invalid.R
* test-356-missingSummary.R
* test-357-prepUserImportData.R



**Survey Methods**

Files 400 - 449

* test-400-exportSurveyParticipants-ArgumentValidation.R
* test-400-exportSurveyParticipants-Functionality.R


**Logging**

Files 800 - 810

* test-800-exportLogging-argumentValidation.R
* test-800-exportLogging-Functionality.R

**Deprecated**

Files 900 - 999

* test-900-checkbox-suffixes.R
* test-901-cleansMetaData.R
* test-902-redcapProjectInfo.R
* test-903-exportBundle.R
* test-904-fieldToVar.R
* test-905-makeRedcapFactor.R
* test-906-massert.R
* test-907-redcapFactorFlip.R
* test-908-syncUnderscoreCodings.R
* test-909-validateImport.R




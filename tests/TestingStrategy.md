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

Files 100 - 199


**Basic Records Import / Export / Deletion**

Files 200 - 299


**Typed Records Export**

Files 300 - 349


**Record Post Processing**

Files 350 - 399


**File Exports**

Files 400 - 449


**Other Methods**

Files 450 - 499



**Survey Methods**

Files 500 - 549


**Logging**

Files 800 - 810

**Deprecated**

Files 900 - 999


## Current Files For Development


**Project Infrastructure**

Files 100 - 199

* test-101-userMethods-Functionality.R
* test-109-instrumentMethods-ArgumentValidation.R
    + includes exportPDF argument validation
* test-109-instrumentMethods-Functionality.R
    + included exportPDF functionality
* test-110-eventMappings-ArgumentValidation.R
* test-110-eventMappings-Functionality.R
* test-111-repeatingInstruments-ArgumentValidation.R
* test-111-repeatingInstruments-Functionality.R

**Basic Records Import / Export / Deletion**

Files 200 - 299

* test-200-recordsMethods-ArgumentValidation.R
* test-200-recordsMethods-Functionality.R
* test-201-recordsWithDags.R
* test-202-recordsWithEvents.R
* test-203-recordsWithSurveys.R
* test-204-recordsWithRepeatingInstruments.R
* test-205-exportRecordsOffline-ArgumentValidation.R
* test-205-exportRecordsOffline-Functionality.R
* test-250-exportReports-ArgumentValidation.R
* test-250-exportReports-Functionality.R

**Typed Records Export**

Files 300 - 349

* test-300-exportRecordsTyped-ArgumentValidation.R
* test-300-exportRecordsTyped-Functionality.R
* test-301-typedRecordsWithDags.R
* test-302-typedRecordsWithEvents.R
* test-303-typedRecordsWithSurveys.R
* test-304-typedRecordsWithRepeatingInstruments.R
* test-305-typedRecordsOffline-ArgumentValidation.R
* test-305-typedRecordsOffline-Functionality.R

**Record Post Processing**

Files 350 - 399

* test-350-recastRecords.R
* test-351-castForImport.R
* test-352-guessCast.R
* test-353-guessDate.R
* test-354-mChoiceCast.R
* test-355-splitForms.R
* test-356-dropRepeatingNA.R

**File Exports**

Files 400 - 449

* test-400-reconstituteFileFromExport.R
* test-401-fileExportMethods-ArgumentValidation.R
* test-401-fileExportMethods-Functionality.R
* test-402-createFileRepository-ArgumentValidation.R
* test-402-createFileRespoitory-Functionality.R
    + Will be an empty file as there is no way to clean up afterward.
* test-403-fileRepository-SingleFileMethods-ArgumentValidation.R
* test-403-fileRepository-SingleFileMethods-Functionality.R
* test-404-fileRepository-BulkFileMethods-ArgumentValidation.R
* test-404-fileRepository-BulkFileMethods-Functionality.R

**Other Methods**

Files 450 - 499

* test-450-exportNextRecordName-ArgumentValidation.R
* test-450-exportNextRecordName-Functionality.R
* test-451-exportVersion.R
* test-452-savePurgeRestoreProject-ArgumentValidation.R
* test-452-savePurgeRestoreProject-Functionality.R
* test-453-allocationTable-ArgumentValidation.R
* test-453-allocationTable-Functionality.R
* test-454-getProjectIdFields.R
* test-455-invalid.R
* test-456-missingSummary.R
* test-457-prepUserImportData.R



**Survey Methods**

Files 500 - 549

* test-500-exportSurveyParticipants-ArgumentValidation.R
* test-500-exportSurveyParticipants-Functionality.R


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




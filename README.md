

[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.11826.png)](https://dx.doi.org/10.5281/zenodo.11826)
![](https://cranlogs.r-pkg.org/badges/grand-total/redcapAPI)

redcapAPI
======

## Quick Start Guide

There are 2 basic functions that are key to understanding the major changes with this version:

* `unlockREDCap`
* `exportBulkRecords`

Here's a typical call for these two:

```
library(redcapAPI)

options(keyring_backend=keyring::backend_file) # Put in .Rprofile

unlockREDCap(c(rcon    = '<MY PROJECT NAME>'),
             keyring     = 'API_KEYs',
             envir       = globalenv(),
             url         = 'https://<REDCAP_URL>/api/')
exportBulkRecords(list(db = rcon),
  forms = list(db = unique(rcon$metadata()$form_name)),
  envir = globalenv())
```

The `<MY PROJECT NAME>` is a reference for whatever you wish to call this REDCap project. The `rcon` is the variable you wish to assign it too. The keyring is a name for this key ring. If one uses `'API_KEYs'` for all your projects, you'll have one big keyring for all your API_KEYs locally encrypted. The url is the standard url for the api. The `passwordFUN` specified is an override if one is using RStudio. It's not required, but on a Mac this is the only option that works well. The `envir` call is where to write the connection object; if not specified the call will return a list.

The next call to `exportBulkRecords`, says to export by form and leave out records not filled out and columns not part of a form. The first argument is specifying a `db` reference to the connection opened and naming it the same thing. The second call is saying for this connection export back the all the forms present in that `db`. The `envir` has it writing it back to the global environment as variables. Any parameter not recognized is passed to the `exportRecordsTyped` call. 

These two calls will handle most analysis requests. To truly understand all these changes see: `vignette("redcapAPI-best-practices")`

## 2.7.0+

2.7.0 includes `exportRecordsTyped` which is a major move forward for the package. It replaces `exportRecords` with a far more stable and dependable call. It includes retries with exponential backoff through the connection object. It has inversion of control over casting, and has a useful validation report attached when things fail. It is worth the time to convert calls to `exportRecords` to `exportRecordsTyped` and begin using this new routine. It is planned that in the next year `exportRecords` will be removed from the package.

## Back Matter

*NOTE*: Ownership transfer of this package to VUMC Biostatistics is complete.

The research community owes a big thanks to [Benjamin Nutter](https://github.com/nutterb/redcapAPI)
for his years of service keeping this package current.

The package `redcapAPI` is an R interface to REDCap (https://www.projectredcap.org/), originally created by [Jeffrey Horner](https://github.com/jeffreyhorner).

Please read the documentation on your institutions REDCap installation.

Issues may be reported at [Issues](https://github.com/vubiostat/redcapAPI/issues)

This package was developed under REDCap Version 13.8.2. Institutions can be a little behind on updating REDCap and so some features of the API may not always work.

### Coding Style Guideline Note

- Exported function names: dromedaryCase
- Internal function names: .dromedaryCase
- Constant data exported: UPPERCASE
- Function parameters: snake_case
- Function variables: snake_case
- - (exception) data.frame variable: CamelCase

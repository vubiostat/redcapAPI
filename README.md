

[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.11826.png)](http://dx.doi.org/10.5281/zenodo.11826)
![](http://cranlogs.r-pkg.org/badges/grand-total/redcapAPI)

redcapAPI
======

2.7.0 includes `exportRecordsTyped` which is a major move forward for the package. It replaces `exportRecords` with a far more stable and dependeable call. It includes retries with exponential backoff through the connection object. It has inversion of control over casting, and has a useful validation report attached when things fail. It is worth the time to convert calls to `exportRecords` to `exportRecordsTyped` and begin using this new routine. It is planned that in the next year `exportRecords` will be removed from the package.

*NOTE*: Ownership transfer of this package to VUMC Biostatistics is complete.
Existing tickets in the older git repo will be transitioned over the next couple months.

The research community owes a big thanks to [Benjamin Nutter](https://github.com/nutterb/redcapAPI)
for his years of service keeping this package current.

The package `redcapAPI` is an R interface to REDCap (http://www.project-redcap.org/), originally created by [Jeffrey Horner](https://github.com/jeffreyhorner).

Please read the documentation on your institutions REDCap installation.

Issues may be reported at [Issues](https://github.com/vubiostat/redcapAPI/issues)

This package was developed under REDCap Version 13.4.3. Institutions can be a little behind on updating REDCap and so some features of the API may not always work.


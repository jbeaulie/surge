# Survey of Reservoir Greenhouse gas Emissions (SuRGE)

## R Project Directory Structure
### Data
The full suite of SuRGE data and project management documentation (e.g., Quality Assurance Project Plan, SOPs) are maintained in the shared documents library at the private SuRGE SharePoint site (https://usepa.sharepoint.com/sites/SuRGE) and can be made available upon request (contact Jake Beaulieu, beaulieu.jake@epa.gov). The subset of the SuRGE data needed to execute scripts in the `scripts/analysis` folder are located in `inputData` and `SuRGE_Sharepoint`. 

## Library Management

This project uses the `renv` library to manage library versions.  After creating a local clone, a message will appear in the console directing users to sync the project library  by running `renv::restore()`.  This will download the packages and versions used in this project as specified in the renv.lock file.   Please see here (https://rstudio.github.io/renv/index.html) for a primer on `renv` and here (https://rstudio.github.io/renv/articles/collaborating.html) for details.

Please use caution when updating libraries already captured in the lock file.  The code runs succesfully with the suite of packages captured in the current lock file; updating package versions could cause problems with the existing code.  The whole point of `renv` is to  minimize the potential for code breakage due to differences among package versions.

The R project was initiated by J Beaulieu using R.4.4.1 and `renv` will produce a warning message if a different version of R is used.  In theory, `renv` shouldn't be sensitive to the version of R, but in practice I have found that `renv::restore()` can take a very long time or even fail if a different version of R is used.

Please note that the .Rprofile file is under version control in the repository.  This file contains an autoloader which automatically downloads and installs the appropriate version of `renv` into the project library.  It is best that users do not use the .Rprofile file to further customize the environment. 

Finally, `renv::restore()` may require Rtools.  Rtools can be found here (https://cran.r-project.org/bin/windows/Rtools/history.html).  Be sure to install a version that is compatible with your R version.

## Conflicted
This project uses `conflicted` to manage conflicts between the `dplyr` functions `select()` and `filter()` and functions with the same names, but in different packages (i.e.`MASS::select()`).  The `conflicted` settings will always give preference to the dplyr library when `select()` or `filter()` are called.

## Running Scripts
Scripts must be run in the sequence documented in scripts/masterScript.R.

## Version Control
This project uses git version control.  git software is independent from the github website and must be installed on each users computer.  Users may submit pull requests if they wish to contribute to the project.

## Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

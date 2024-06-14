This repository houses data, code, documentation, and report files for the Caribbean Ecosystem Status Report created by the NOAA Fisheries Southeast Fisheries Science Center. We are currently working on the 2024 ESR.

The list of indicators included in the 2024 ESR can be found in this [table](https://docs.google.com/spreadsheets/d/1WZtclTkyLzTAARKTIa69AiEVWsXMuG2K/edit?usp=sharing&ouid=103178636955659669576&rtpof=true&sd=true). This table also includes information about data sources, indicator status, and automation level. Eventually all of this information will be migrated to an indicator [catalog](https://github.com/Gulf-IEA/ESR-indicator-catalog.git). The catalog is still a work in progress.

There are several folders in this repository.
1. indicator_data --> houses all the non-confidential data needed to create each indicator. Confidential data are stored elsewhere.
2. indicator_processing --> houses all the code (R scripts) for creating each indicator. The CalculateAllIndicators.R file runs all of the R scripts for all indicators. This should be run anytime the report needs to be updated. 
3. indicator_objects --> houses all of the .RData outputs from the R scripts used to create each indicator. These are tables all in a standardized format with each indicator variable and a time column. This folder also contains the PlotAllIndicators.R file, which creates all of the plots for the ESR and saves them to the indicator_plots folder. This script is a work in process, it does not yet run.
4. indicator_plots --> houses all the output plots from the PlotAllIndicators.R script as well as any additional plots that are created separately to be used in the report.
5. Report_book_files --> houses the quarto book document and associated files needed to create the ESR report.
6. _book --> contains the rendered pdf version of the ESR report. This again is a work in progress.


_____________________________________________________________________

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government. 

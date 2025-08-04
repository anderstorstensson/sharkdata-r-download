# Repository Archived

This repository has been archived as SHARKdata has been replaced by a new API. For accessing data via the new API, please refer to [SHARK4R](https://github.com/sharksmhi/SHARK4R).


# sharkdata-r-download
Access physical-chemical, phytoplankton, picoplankton and chlorophyll data from the [SHARKdata](https://sharkdata.smhi.se/) API using R. The data are also available in the web-interface at [SHARKweb](https://sharkweb.smhi.se/hamta-data/).

There are three scripts available in this repo:

## download_shark_data_general.Rmd 
Simple version

## download_shark_data.Rmd
Added [WoRMS](https://www.marinespecies.org/) taxonomy for phytoplankton

## download_shark_data_updated_taxonomy.Rmd
Added [WoRMS](https://www.marinespecies.org/) taxonomy for phytoplankton and updated taxonomy to follow the current [AlgaeBase taxonomic database](https://www.algaebase.org/) and better match the [PR2 database, v5.01](https://pr2-database.org/) (both accessed on 2023-06-02).

## Instructions

List the dataset names that should be downloaded in data/shark_download/list_of_datasets.txt.

## Citation
Torstensson, A. (2023). Access Swedish oceanographic monitoring data using the SHARKdata API (https://sharkdata.smhi.se/) in R (Version 1.1) [Computer software]. https://doi.org/10.5281/zenodo.8096047

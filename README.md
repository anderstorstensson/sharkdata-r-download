# sharkdata-r-download
Download physical-chemical, phytoplankton, picoplankton and chlorophyll data from the [SHARKdata](https://sharkdata.smhi.se/) API using R

There are three scripts available in this repo:

## download_shark_data_general.Rmd 
Simple version

## download_shark_data.Rmd
Added [WoRMS](https://www.marinespecies.org/) taxonomy for phytoplankton

## download_shark_data_updated_taxonomy.Rmd
Added [WoRMS](https://www.marinespecies.org/) taxonomy for phytoplankton and updated taxonomy to follow the current [AlgaeBase taxonomic database](https://www.algaebase.org/) and better match the [PR2 database, v5.01](https://pr2-database.org/) (both accessed on 2023-06-02).

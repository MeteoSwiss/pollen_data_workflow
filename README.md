This repository contains:
A. 4 protocol files:
- 2 protocol form files for collecting pollen on the field (.docx and .pdf versions)
- 2 protocol procedure files explaining the steps of collecting, aerosolising and measuring pollen (.docx and .pdf versions)

B. 2 R code files:
- read_zipfile: extracts the .json files from the zip files (1 zip file = 1 dataset), then reads the .json files and creates an .Rdata dataframe with all the .json information for all datasets
- explore_data: based on the json dataframe created with read_zipfile.R, summarises the data information, filters the data and creates PCA plots

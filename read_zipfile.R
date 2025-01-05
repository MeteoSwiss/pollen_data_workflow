
# -------------------------------------------------------------------------

# GOAL: read the datasets .zip files and extract the .json files from them

# -------------------------------------------------------------------------

# clear your environment
rm(list = ls())

# packages ----------------------------------------------------------------

library(rjson)
library(dplyr)
library(purrr)
library(doParallel)
library(furrr) # parallelise map()


# working directory path --------------------------------------------------
### TO CHANGE
mypath <- "your_path/MCH_datasets_pollen_2023-2024"


# list all zip files ------------------------------------------------------
zippath <- paste(mypath, "/data_zip_files", sep = "")

zipfiles <- list.files(paste(zippath), pattern = ".zip")


# function to extract .json files in a zip --------------------------------
extractzip <- function(x) { # x = 1 zip file
  
  # list .json files, list = T means no extract
  jsonlist <- dplyr::filter(unzip(paste(zippath, x, sep = ""), list = T),
                     grepl('.json', Name))
  
  # temporary extract .json from zip file ---------------------------------------------
  unzip(paste(zippath, x, sep = ""), 
        exdir = paste(mypath, "/temp_extract/", 
                      stringr::str_split_1(x, ".zip")[1], 
                      sep = ""), 
        files = jsonlist$Name)
  
}

# run .json extract in parallel -------------------------------------------
### runs on a regular PC but requires a few hours

parallel::detectCores() # number of cores available
n.cores <- parallel::detectCores() - 1 # keep 1 core free

#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

#check cluster definition
print(my.cluster)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#check if it is registered
foreach::getDoParRegistered()
#how many workers are available?
foreach::getDoParWorkers()

#run in parallel
foreach (i = 1:length(zipfiles)) %dopar% {
  extractzip(zipfiles[i])
}


# function to read all .json file in one zip and create a single table ------------

parsefile <- function(y) { # y = 1 unziped file in temp_extract
  
  jsonlist <- list.files(path = paste(mypath, "/temp_extract/", y, "/", sep = ""))
  
  # function to parse one .json file and return a df ----------------------------
  parsejson <- function(x) { # x = 1 json file
    # read .json files
    jsondata <- fromJSON(file = paste(mypath, "/temp_extract/", y, "/", x, sep = ""))
    
    # function to return NA to fill the df if we have a NULL
    checkifnull <- function(x){
      if (is.null(x))
        return(NA)
      else
        return(x)
    }
    
    # create df with necessary json information, if no measure -> feature = NA
    jsondf <- data.frame(
        #eventID
        eventID = pluck(jsondata$timestamp_dt) %>% checkifnull(),
        #measurement campaign, !! wrong names in the json files!!, use the folder name
        meas_camp = y,
        #image features
        area = pluck(jsondata$computed_data$holography$image_pairs, 1, 1)$rec_mag_properties$area %>% checkifnull(),
        eccentricity = pluck(jsondata$computed_data$holography$image_pairs, 1, 1)$rec_mag_properties$eccentricity %>% checkifnull(),
        equivalent_diameter = pluck(jsondata$computed_data$holography$image_pairs, 1, 1)$rec_mag_properties$equivalent_diameter %>% checkifnull(),
        major_axis_length = pluck(jsondata$computed_data$holography$image_pairs, 1, 1)$rec_mag_properties$major_axis_length %>% checkifnull(),
        minor_axis_length = pluck(jsondata$computed_data$holography$image_pairs, 1, 1)$rec_mag_properties$minor_axis_length %>% checkifnull(),
        max_intensity = pluck(jsondata$computed_data$holography$image_pairs, 1, 1)$rec_mag_properties$max_intensity %>% checkifnull(),
        min_intensity = pluck(jsondata$computed_data$holography$image_pairs, 1, 1)$rec_mag_properties$min_intensity %>% checkifnull(),
        perimeter = pluck(jsondata$computed_data$holography$image_pairs, 1, 1)$rec_mag_properties$perimeter %>% checkifnull(),
        solidity = pluck(jsondata$computed_data$holography$image_pairs, 1, 1)$rec_mag_properties$solidity %>% checkifnull(),
        #relative fl, source LED 280nm and channel 357nm, etc.
        fl280_357nm = pluck(jsondata$computed_data$fluorescence$processed_data$spectra$relative_spectra, 1, 1) %>% checkifnull(),
        fl280_435nm = pluck(jsondata$computed_data$fluorescence$processed_data$spectra$relative_spectra, 1, 2) %>% checkifnull(),
        fl280_483nm = pluck(jsondata$computed_data$fluorescence$processed_data$spectra$relative_spectra, 1, 3) %>% checkifnull(),
        fl280_562nm = pluck(jsondata$computed_data$fluorescence$processed_data$spectra$relative_spectra, 1, 4) %>% checkifnull(),
        fl280_676nm = pluck(jsondata$computed_data$fluorescence$processed_data$spectra$relative_spectra, 1, 5) %>% checkifnull(),
        fl365_357nm = pluck(jsondata$computed_data$fluorescence$processed_data$spectra$relative_spectra, 2, 1) %>% checkifnull(),
        fl365_435nm = pluck(jsondata$computed_data$fluorescence$processed_data$spectra$relative_spectra, 2, 2) %>% checkifnull(),
        fl365_483nm = pluck(jsondata$computed_data$fluorescence$processed_data$spectra$relative_spectra, 2, 3) %>% checkifnull(),
        fl365_562nm = pluck(jsondata$computed_data$fluorescence$processed_data$spectra$relative_spectra, 2, 4) %>% checkifnull(),
        fl365_676nm = pluck(jsondata$computed_data$fluorescence$processed_data$spectra$relative_spectra, 2, 5) %>% checkifnull(),
        fl405_357nm = pluck(jsondata$computed_data$fluorescence$processed_data$spectra$relative_spectra, 3, 1) %>% checkifnull(),
        fl405_435nm = pluck(jsondata$computed_data$fluorescence$processed_data$spectra$relative_spectra, 3, 2) %>% checkifnull(),
        fl405_483nm = pluck(jsondata$computed_data$fluorescence$processed_data$spectra$relative_spectra, 3, 3) %>% checkifnull(),
        fl405_562nm = pluck(jsondata$computed_data$fluorescence$processed_data$spectra$relative_spectra, 3, 4) %>% checkifnull(),
        fl405_676nm = pluck(jsondata$computed_data$fluorescence$processed_data$spectra$relative_spectra, 3, 5) %>% checkifnull()
      )
    
  return(jsondf)
}

temp_df <- purrr::map_df(jsonlist, parsejson)

return(temp_df)

}

# list all files in the temp_extract folder
files <- list.files(paste(mypath, "/temp_extract", sep = ""))

# run in parallel
### workers to adapt to your PC

future::plan(cluster, workers = 3)
df <- furrr::future_map_dfr(files, parsefile, .progress = T)


save(df, file = paste(mypath, "/code_output/jsondf.RData", sep = ""))


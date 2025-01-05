

# -------------------------------------------------------------------------

# GOAL: explore the .json files content: format and summarise the data + PCA

# -------------------------------------------------------------------------

# clear your environment
rm(list = ls())

# set your directory path
### TO CHANGE
mypath <- "your_path/MCH_datasets_pollen_2023-2024"

# load the json dataframe file jsondf.RData (= output of the read_zipfile.R code), also directly available in Zenodo
load(paste(mypath, "/code_output/jsondf.RData", sep = ""))

# packages ----------------------------------------------------------------

library(dplyr)
library(purrr)
library(tidyr)
library(knitr)
library(gt)
library(readxl)


# create a summary table for each dataset/species and variable ------------

# remove fl365_357nm and fl405_357nm since the source saturates the channel
df$fl365_357nm <- df$fl405_357nm <- NULL

# add genus, species, lat, long, etc. columns according to the metadata .xksx file
plant_meta <- read_xlsx(path = paste(mypath, "/MCH_metadata_pollen_2023-2024.xlsx", sep = ""), sheet = "Plant")
sample_meta <- read_xlsx(path = paste(mypath, "/MCH_metadata_pollen_2023-2024.xlsx", sep = ""), sheet = "Sample")
match_meta <- plant_meta %>% left_join(sample_meta, by = "plant_ID")
df <- df %>% left_join(match_meta, by = join_by("meas_camp" == "sample_name"))

# transform into factors
df$meas_camp <- as.factor(df$meas_camp)
df$genus <- as.factor(df$genus)
df$species <- as.factor(df$species)
df$authorship <- as.factor(df$authorship)

str(df)


# filter data -> delete no image = only NA image features -----------------
# keep events that have at least one image feature that is not NA and full FL (all sum(is.na(df1$fl365_676nm)) = 0)
### to modify for filtering (cleaning) the data as you wish
df1 <- df %>% 
  filter(!if_all(c(area, 
                   eccentricity, 
                   equivalent_diameter, 
                   major_axis_length, 
                   minor_axis_length, 
                   max_intensity, 
                   perimeter, 
                   solidity), ~ is.na(.))) %>% 
  filter(max_intensity > 0)


# list of ids of events to remove (no img) --------------------------------
# also available directly in Zenodo
rm_ids <- df %>% 
  filter(if_all(c(area,
                   eccentricity,
                   equivalent_diameter,
                   major_axis_length,
                   minor_axis_length,
                   max_intensity,
                   perimeter,
                   solidity), ~ is.na(.)) | max_intensity <= 0 ) %>% 
  subset(select = c("eventID", "meas_camp"))
save(rm_ids, file = paste(mypath, "/code_output/rm_ids.Rdata", sep = ""))
  

# summary table -----------------------------------------------------------

# summary statistics by campaign : num event, complete/incomplete events, genus, species, authorship, lat, long
summ <- df1 %>% 
  group_by(meas_camp) %>% 
  summarise(complete_events=n()) %>% 
  left_join(df1 %>% 
              select(meas_camp,
                     genus,
                     species,
                     authorship) %>% 
              distinct()) %>%
  left_join(df %>% 
              group_by(meas_camp) %>%
              summarise(events=n())
  ) %>% 
  mutate(incomplete_events =events-complete_events) %>% 
  select(-events)

summ %>%
  gt() %>% 
  tab_header(title = "List of datasets") %>% 
  cols_move_to_end(c(complete_events, incomplete_events)) %>%
  write.csv(paste(mypath, "/code_output/summary_table.csv", sep = ""))


# interactive PCA on medians ----------------------------------------------------------

library(Factoshiny)
library(ggpubr)

df1 %>% 
  group_by(meas_camp) %>%
  select(-c(latitude, longitude, ID)) %>% 
  summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE), .names = "{.col}")) %>% 
  left_join(df1 %>% 
              select(meas_camp,
                     genus,
                     species) %>% 
              distinct()) %>%
  PCAshiny()


# PCA dim1 and 2 plots ----------------------------------------------------

nb <- missMDA::estim_ncpPCA(df1 %>% group_by(meas_camp) %>% select(-c(latitude, longitude, ID)) %>% summarise(across(where(is.numeric), ~median(.x, na.rm = TRUE), .names = "{.col}")) %>% left_join(df1 %>% select(meas_camp, genus, species) %>% distinct()),quali.sup=c(1,24,25))$ncp
dfcompleted <- missMDA::imputePCA(df1 %>% group_by(meas_camp) %>% select(-c(latitude, longitude, ID)) %>% summarise(across(where(is.numeric), ~median(.x, na.rm = TRUE), .names = "{.col}")) %>% left_join(df1 %>% select(meas_camp, genus, species) %>% distinct()),ncp=nb,quali.sup=c(1,24,25))$completeObs
res.PCA<-PCA(dfcompleted,quali.sup=c(1,24,25),graph=FALSE)
# change axes to change dim
v <- plot.PCA(res.PCA,axes=c(1,2),choix='var',title="PCA variables")
v

i <- plotellipses(res.PCA,keepvar=24,axes=c(1,2),title="PCA individuals",label =c('ind','quali')) +
  theme(legend.position = "none")
i

ggsave(plot = v, paste(mypath, "/code_output/figures/var_pca_dim1-2.pdf", sep = ""), width = 1920/80 , height = 1080/80, units = "cm")
ggsave(plot = i, paste(mypath, "/code_output/figures/indiv_pca_dim1-2.pdf", sep = ""), width = 1920/80 , height = 1080/80, units = "cm")


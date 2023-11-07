#Prepping environmental data for stability SAM
#Ana Miller-ter Kuile
#June 26, 2023

#this is a script that can prep environmental
#data for the SAM model

# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 

package.list <- c("here", "tidyverse",
                  "data.table")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

## And loading them

for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

#giant kelp biomass (yearly)

#bottom temperature (every 15 minutes - maybe 
#summarise monthly for now? and later think about
#more biologically-relevant "seasons"?)

biomass <- read.csv(here('fish_example',
                         'data',
                         'data_raw',
                         "Annual_All_Species_Biomass_at_transect_20230814.csv"))

#from temp "data prep" script for SAM. these are HUGE datasets
#so didn't want to put summary in here because it takes forever
bottemp <- read.csv(here('fish_example',
                         'data',
                         'data_raw',
                         "seasonal_bottom_temps.csv"))

#to get the sites and transects we need for the other 
#two datasets
stability <- readRDS(here('fish_example',
                          'data',
                          'data_raw',
                          "fish_bray_meanSD.RDS"))

IDs <- read.csv(here('fish_example',
                     'data',
                     'data_raw',
                     "site_year_IDs.csv"))
# Biomass by Year by Site -------------------------------------------------

colnames(biomass)
#i'll filter only giant kelp "MAPY"
#and the dry biomass value "DRY_GM2"

biomass <- biomass %>%
  dplyr::select(YEAR, MONTH, SITE, 
                TRANSECT, SP_CODE, DRY_GM2) %>%
  filter(SP_CODE == "MAPY")


# Prep stability dataset --------------------------------------------------

stability2 <- as.data.frame(stability) %>%
  rownames_to_column(var = "var") %>%
  filter(var != "deviance") %>%
  separate(var,
           into = c('siteID', 'yrID'),
           sep = ",") %>%
  mutate(siteID = str_sub(siteID, 6, nchar(siteID))) %>%
  mutate(yrID = str_sub(yrID, 1, (nchar(yrID)-1))) %>%
  rename("bray" = "Mean") %>%
  dplyr::select(yrID, siteID, bray, SD) %>%
  mutate(yrID = as.numeric(yrID),
         siteID = as.numeric(siteID)) %>%
  left_join(IDs, by = c("siteID", "yrID"))


# Get site and transect IDs -----------------------------------------------

sites <- stability2 %>%
  distinct(siteID, SITE_TRANS) %>%
  separate(SITE_TRANS, into= c("SITE", "TRANSECT"),
           sep = "_",
           remove = F) 

site <- sites$SITE
sitetrans <- sites$SITE_TRANS

# Filter environmental to sites and transects in dataset ------------------

bottemp2 <- bottemp %>%
  filter(SITE %in% site)

bottemp2 %>%
  group_by(SEASON) %>%
  summarise(mean = mean(TEMP_C))



biomass2 <- biomass %>%
  unite(c(SITE, TRANSECT),
        col = "SITE_TRANS",
        sep = "_",
        remove = F) %>%
  filter(SITE_TRANS %in% sitetrans) %>%
  mutate(DRY_GM2 = case_when(DRY_GM2 == -99999 ~ NA_real_,
                             TRUE ~ DRY_GM2))

# Make Lags ---------------------------------------------------------------

#biomass2:
#yearly lags - maybe start with 5-6 years back?
#site, transect, site_Transect, year
#Bio_1, lag back 5 years

bio_lags <- biomass2 %>%
  group_by(SITE_TRANS) %>%
  arrange(SITE_TRANS, YEAR) %>%
  #this creates a column for every lag 1:6 years ago
  do(data.frame(., setNames(shift(.$DRY_GM2, 1:5), c("DRY_GM2_l1",
                                                 "DRY_GM2_l2", "DRY_GM2_l3",
                                                 "DRY_GM2_l4", "DRY_GM2_l5")))) %>%
  ungroup() %>%
  dplyr::select(YEAR, SITE_TRANS, SITE,
                TRANSECT, DRY_GM2:DRY_GM2_l5)

#temperatuer lags - seasonally - going back 
#how many seasons now??? HMMM....

temp_lags <- bottemp2 %>%
  group_by(SITE) %>%
  arrange(SITE, YEAR, SEASON) %>%
  #this creates a column for a lag 1:5 seasons ago
  do(data.frame(., setNames(shift(.$TEMP_C, 1:5), c("TEMP_C_l1",
                                                     "TEMP_C_l2", "TEMP_C_l3",
                                                     "TEMP_C_l4", "TEMP_C_l5")))) %>%
  ungroup() %>%
  dplyr::select(SITE, YEAR, SEASON,
                TEMP_C:TEMP_C_l5)

#set first season of temp lags to be 
#"WARM" since this is when all the surveys took place
temp_lags2 <- temp_lags %>%
  filter(SEASON == "WARM") %>%
  dplyr::select(-SEASON) %>%
  mutate(YEAR = as.integer(YEAR))



# Combine all data --------------------------------------------------------

all_data <- stability2 %>%
  left_join(bio_lags, by = c("YEAR",
                             "SITE_TRANS")) %>%
  left_join(temp_lags2, by = c("SITE", "YEAR")) 
  

write.csv(all_data, here('fish_example',
                         'data',
                         'data_output',
                         "stability_metrics_with_covariates.csv"))



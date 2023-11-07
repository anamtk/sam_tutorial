#Prepping data object for the stability SAM models
#Ana Miller-ter Kuile
#July 5, 2023

#this is a script that preps data list for the turnover SAM models

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

all_data <- read.csv(here('fish_example',
                          'data',
                          'data_output',
                          "stability_metrics_with_covariates.csv"))

# Prep data for jags ------------------------------------------------------

n.data <- nrow(all_data)

bray <- as.vector(all_data$bray)
var.estimate <- as.vector(all_data$SD^2)

n.transects <- length(unique(all_data$SITE_TRANS))

Transect.ID <- all_data$siteID

n.years <- length(unique(all_data$YEAR))

Year.ID <- all_data$yrID

n.sites <- length(unique(all_data$SITE))

Site.ID <- all_data %>%
  distinct(siteID, SITE) %>%
  arrange(siteID) %>%
  mutate(SITE = as.numeric(as.factor(SITE))) %>%
  dplyr::select(SITE) %>%
  as_vector()

n.kelplag <- all_data %>%
  dplyr::select(DRY_GM2:DRY_GM2_l5) %>%
  ncol()

Kelp <- all_data %>%
  dplyr::select(SITE_TRANS, YEAR, DRY_GM2:DRY_GM2_l5) %>%
  pivot_longer(DRY_GM2:DRY_GM2_l5,
               names_to = 'lag',
               values_to = 'kelp') %>%
  mutate(kelp = scale(kelp)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "kelp") %>%
  dplyr::select(DRY_GM2:DRY_GM2_l5) %>%
  as.matrix()

sum(is.na(Kelp))
sum(!is.na(Kelp))

#~10% missing data

n.templag <- all_data %>%
  dplyr::select(TEMP_C:TEMP_C_l5) %>%
  ncol()

Temp <- all_data %>%
  dplyr::select(SITE_TRANS, YEAR, TEMP_C:TEMP_C_l5) %>%
  pivot_longer(TEMP_C:TEMP_C_l5,
               names_to = 'lag',
               values_to = 'temp') %>%
  mutate(temp = scale(temp)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "temp") %>%
  dplyr::select(TEMP_C:TEMP_C_l5) %>%
  as.matrix()

sum(is.na(Temp))/(sum(is.na(Temp)) + sum(!is.na(Temp)))
#~7% missing data

data <- list(n.data = n.data,
             n.transects = n.transects,
             n.sites = n.sites,
             n.years = n.years,
             Transect.ID = Transect.ID,
             Year.ID = Year.ID,
             Site.ID = Site.ID,
             bray = bray,
             var.estimate = var.estimate,
             n.kelplag = n.kelplag,
             Kelp = Kelp,
             n.templag = n.templag,
             Temp = Temp)

saveRDS(data, here('fish_example',
                   'data',
                   'data_output',
                   "bray_SAM_input_data.RDS"))



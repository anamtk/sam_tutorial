# Data prep for initiation date JAGS model
# Ana Miller-ter Kuile
# November 8, 2022

# this script preps data for the JAGS model for nest initation day


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse",
                  "readxl")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

source(here('functions',
            'tidy_functions.R'))

# Load data ---------------------------------------------------------------

init <- read.csv(here("woodpecker_example", 
                      "data",
                      "data_raw",
                      "Nest_initiation_data.csv"))

# Numbers for loops -------------------------------------------------------

n.nests <- nrow(init)

n.transects <- init %>%
  distinct(Transect_ID2) %>%
  summarise(n.transects = n()) %>%
  dplyr::select(n.transects) %>%
  as_vector()

n.years <- init %>%
  distinct(Year_located) %>%
  summarise(n.years = n()) %>%
  dplyr::select(n.years) %>%
  as_vector()

n.forests <- init %>%
  distinct(Project_ID) %>%
  summarise(n.forests = n()) %>%
  dplyr::select(n.forests) %>%
  as_vector()

n.lag <- init %>%
  dplyr::select(Tmax:Tmax_l9) %>%
  ncol() 

# Numbered factors for transects and years --------------------------------

Transect.num <- init %>%
  dplyr::select(Transect_ID2) %>%
  as_vector() %>%
  nums()

Year.num <- init %>%
  dplyr::select(Year_located) %>%
  as_vector() %>%
  nums()

Forest.num <- init %>%
  dplyr::select(Project_ID) %>%
  as_vector() %>%
  nums()

# Main effects ------------------------------------------------------------
#scale all Temp data to same scale
Tmax <- init %>%
  dplyr::select(Nest_ID, Tmax:Tmax_l9) %>% #adjust if needed
  pivot_longer(Tmax:Tmax_l9,
               names_to = "lag",
               values_to = "temp") %>%
  mutate(temp = scale(temp)) %>%
  pivot_wider(names_from = "lag",
              values_from = "temp") %>%
  dplyr::select(-Nest_ID) %>%
  as.matrix()

PPT <- init %>%
  dplyr::select(Nest_ID, PPT:PPT_l10) %>%
  pivot_longer(PPT:PPT_l10,
               names_to = "lag",
               values_to = "ppt") %>%
  mutate(ppt = scale(ppt)) %>%
  pivot_wider(names_from = "lag",
              values_from = "ppt") %>%
  dplyr::select(-Nest_ID) %>%
  as.matrix()


# Response data -----------------------------------------------------------

y <- log(init$Init_day)

# Compile and export ------------------------------------------------------

all_data <- list(n.nests = n.nests,
                 n.transects = n.transects,
                 n.years = n.years,
                 Transect.num = Transect.num,
                 Year.num = Year.num,
                 Tmax = Tmax,
                 PPT = PPT,
                 n.lag = n.lag,
                 y = y,
                 n.forests = n.forests,
                 Forest.num = Forest.num)

saveRDS(all_data, here("woodpecker_example",
                       "data",
                       "data_output",  
                       "JAGS_input_data_list.RDS"))


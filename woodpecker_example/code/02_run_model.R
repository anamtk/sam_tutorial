# Running the nest initiation
# Ana Miller-ter Kuile
# November 4, 2021

# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "jagsUI",
                  'rjags',
                  'mcmcplots',
                  "coda") #mcmc output


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# source(here("code", 
#             "00_functions",
#             "plot_functions.R"))

# Load Data ---------------------------------------------------------------

#load the formatted data for the JAGS model
data <- readRDS(here("woodpecker_example",
                     "data",
                     "data_output",  
                     "JAGS_input_data_list.RDS"))

# Parameters to save ------------------------------------------------------

params <- c("b",
            "b0.transect",
            "b0.year",
            'sig.transect',
            'sig.year',
            "b0",
            'wA',
            'wB',
            'z')


# JAGS model --------------------------------------------------------------

model <- here('woodpecker_example',
              "code", 
              'jags',
              "model.R")


init_mod <- jagsUI::jags(data = data,
                        inits = NULL,
                        model.file = model,
                        parameters.to.save = params,
                        parallel = TRUE,
                        n.chains = 3,
                        n.iter = 4000,
                        DIC = TRUE)

# Check convergence -------------------------------------------------------

parms <- c("b", 
           "b0", 
           "sig.transect", 
           "sig.year", 
           "wA", 
           'wB',
           "deviance")

mcmcplot(init_mod$samples, parms = parms)

gelman.diag(init_mod$samples)
# Raftery -----------------------------------------------------------------

raf_init <- raftery.diag(init_mod$samples)

names <- rownames(raf_init[[1]]$resmatrix)
ch1 <- raf_init[[1]]$resmatrix[,2]
ch2 <- raf_init[[2]]$resmatrix[,2]
ch3 <- raf_init[[3]]$resmatrix[,2]

raf_all <- as.data.frame(cbind(names, 
                               ch1, ch2, ch3)) %>%
  mutate(ch1 = as.numeric(ch1),
         ch2 = as.numeric(ch2),
         ch3 = as.numeric(ch3)) %>%
  filter(!str_detect(names, "z")) %>%
  pivot_longer(ch1:ch3,
               names_to = "chain",
               values_to = 'iterations') 

ggplot(raf_all, aes(x = iterations/3)) +
  geom_histogram() 

raf_all %>%
  summarise(iterations_90 = quantile(iterations, 
                                     probs = 0.9, 
                                     na.rm = T)/3,
            iterations_95 = quantile(iterations,
                                     probs = 0.95,
                                     na.rm = T)/3,
            max = max(iterations, 
                      na.rm = T)/3)
# A tibble: 1 × 3
# iterations_90 iterations_95   max
# <dbl>         <dbl> <dbl>
#   1       39776.        50261. 138516

bu1 <- raf_init[[1]]$resmatrix[,1]
bu2 <- raf_init[[2]]$resmatrix[,1]
bu3 <- raf_init[[3]]$resmatrix[,1]

burn <- as.data.frame(cbind(names, bu1, bu2, bu3)) %>%
  mutate(bu1 = as.numeric(bu1),
         bu2 = as.numeric(bu2),
         bu3 = as.numeric(bu3)) %>%
  filter(!str_detect(names, "z")) %>%
  pivot_longer(bu1:bu3,
               names_to = "chain",
               values_to = 'iterations') 

burn %>%
  summarise(max(iterations, na.rm = T))
#646



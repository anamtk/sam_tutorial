
# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "jagsUI",
                  'rjags',
                  'mcmcplots',
                  "coda",
                  'patchwork') #mcmc output


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

data_list <- readRDS(here('fish_example',
                          'data',
                          'data_output',
                          "bray_SAM_input_data.RDS"))


# Parameters to save ------------------------------------------------------

params <- c('b0.transect',
            'b',
            'wA',
            'wB',
            'sig.transect',
            'sig.site',
            'var.process')



# JAGS model --------------------------------------------------------------

model <- here('fish_example',
              'code',
              'jags',
              "fish_SAM.R")

Sys.time()
mod <- jagsUI::jags(data = data_list,
                    inits = NULL,
                    model.file = model,
                    parameters.to.save = params,
                    parallel = TRUE,
                    n.chains = 3,
                    n.iter = 1,
                    DIC = TRUE)

Sys.time()

# Check convergence -------------------------------------------------------

mcmcplot(mod$samples)

gelman.diag(mod$samples, multivariate = F)


# Look at some plots ------------------------------------------------------

sum <- summary(mod$samples)

med <- as.data.frame(sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  dplyr::select(parameter, `2.5%`, `50%`, `97.5%`) %>%
  filter(parameter != "deviance")

# Quick plots -------------------------------------------------------------

med2 <- med %>%
  filter(parameter %in% c("b[1]", 'b[2]'))

ggplot(med2, aes(x = parameter, y= `50%`)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point() +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0.2) +
  labs(y = "Median (and 95% BCI)", x = "Covariate") +
  scale_x_discrete(labels = c("Kelp", "Temperature")) +
  coord_flip()

weights <- med %>%
  filter(str_detect(parameter, "wB"))

1/6
ggplot(weights, aes(x = parameter, y = `50%`)) +
  geom_hline(yintercept = 1/6, linetype = 2) +
  geom_point() +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0.2) +
  scale_x_discrete(labels = c("Current season (summer)", "Winter - 1",
                              "Summer - 1", "Winter - 2",
                              "Summer - 2", "Winter - 3")) +
  theme(axis.text.x = element_text(angle = 45, hjust =1))

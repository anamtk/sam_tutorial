# Tidying functions
# Ana Miller-ter Kuile
# September 21, 2021

# useful functions for dealing with the data from the 
# WHWO sites

# Load packages -----------------------------------------------------------


package.list <- c("here", "tidyverse", 
                  "readxl", 'stringr')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Insert a character into a string ----------------------------------------

fun_insert <- function(x, pos, insert) {
  #define the position where the new character shoudld "pos" argument
  gsub(paste0("^(.{", pos, "})(.*)$"), 
       #define what characters to insert at that position with 'insert' arg
       paste0("\\1", insert, "\\2"),
       #what should we add the character to? vector/column/etc
       x)
}

# Extract numbers from a text string --------------------------------------

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

# Compute rolling maximum Temperature -------------------------------------

#adapted from heili lowman on kelp project 2022
#this function gets a value for maximum temperature
# from my dataframe based on a defined window for each
# observation
#It takes as arguments a point id, start and end dates
t_max <- function(pointID, start_date, end_date){
  
  dat <- monthly_t %>%
    #get a measurement ID that matches the point ID
    filter(MeasurementID == pointID) %>%
    #set to be just the year in which we want to extract climate
    #which is the last four digits of the MeasurementID column
    filter(ClimateYear == as.numeric(str_sub(MeasurementID, -4, length(MeasurementID)))) %>%
    #get values between the start and end months for the window
    filter(Month >= start_date & Month <= end_date)
  
  #return the maximum value for Tmax, removing NA values
  return(max(dat$Tmax, na.rm = TRUE))
  
}


# Compute a moving window of cumulative precip ----------------------------

ppt_sum <- function(pointID, start_date, end_date){
  
  dat <- monthly_p %>%
    filter(MeasurementID == pointID) %>%
    filter(ClimateYear == as.numeric(str_sub(MeasurementID, -4, length(MeasurementID)))) %>%
    filter(Month >= start_date & Month <= end_date)
  #filter(Month >= start_date & Month <= end_date)
  
  return(sum(dat$PPT, na.rm = T))
  
}

# Get a factored numbered vector for models -------------------------------

#this function helps to get at things like, "what is 
# the year number for nest number 5, etc. which is 
# the preferred data structure for JAGS for 
# factor data

nums <- function(vec){
  
  #from the vector created of each row's 
  # ID - then convert that to numeric
  # based on factor levels
  vec <- as.numeric(as.factor(vec))
  
  return(vec)
}


# Interval covariate Matrix FUnction --------------------------------------

#get variables that span across intervals into 
# a matrix format
# this is for the nest survival model, where we 
# have a matrix of nests x visit intervals
# and some covariates that correspond to each of those
#intervals, such as nest stage

cov_matrix <- function(df, var){
  mat <- df %>%
    #select the nest_ID column and the variable of interest
    dplyr::select(Nest_ID, var) %>%
    #group by each nest
    group_by(Nest_ID) %>%
    #create a visit interval for each nest grouping
    mutate(interval = row_number()) %>%
    ungroup() %>%
    #make into a matrix so that intervals are columns
    pivot_wider(names_from = "interval",
                values_from = all_of(var)) %>%
    #make the nest ID the rowname
    column_to_rownames('Nest_ID') %>%
    #turn integers to numeric
    mutate_if(is.integer, as.numeric) %>%
    #make into a matrix for jags
    data.matrix()
  
  return(mat)
}


# Scaled-unscaled dataframe for predictive graphing -----------------------


#get a dataframe to predict onto that includes
# the original scale of the variable 
# the scaled variable, and the "level"
# of the scaled variable. You can
# specify different "lengths", but 20 is a good
# length to choose from 

#x =the original variable
#length = the lenght of the scale (how many values to predict onto)
# name = the name of the variable that you want to attach to the final df
scale_df <- function(x, length, name){
  
  #get the sequenceo f the variable
  var1 <- seq(min(x, na.rm = T),
              max(x, na.rm = T),
              #set the length you want
              length.out = length)
  
  #scale that variable
  varS <- (var1 - mean(x, na.rm = T))/sd(x, na.rm = T)
  
  #make the OG and scaled a DF with appropriate 
  # names and a "level" for graphing later
  final_df <- var1 %>%
    cbind(varS) %>%
    as.data.frame() %>%
    dplyr::rename(!!name := ".") %>%
    dplyr::mutate(level = row_number())
  
  #get the final DF back from the function
  return(final_df)
}


# DF of parameter medians and CIs for a whole model -----------------------


#takes arguments of an mcmc object and the parameters
# you want to plot
all_estimates_df <- function(mcmc, 
                          params){
  
  #get all the median parameter estimates
  median <- mcmc$q50
  #and filter those to the parameters you want plotted
  median <- median[names(median) %in% params] 
  
  #make this list of medians, which could be vectors 
  # of multiple dimensions - and turn them 
  # into one dataframe where anything with more than
  # one median value gets multiple rows
  median1 <- rbindlist(
    #make the median object into a datatable
    lapply(median, function(x) data.table(t(x))),
    fill = TRUE, idcol = TRUE
  ) %>%
    #general function to fix when a variable
    # has more than one median value column, which will
    # be medians for categorical variables for which
    # the first level will be NA based on model structure
    mutate(V1 = case_when(is.na(V2) ~ V1,
                          !is.na(V2) ~ NA_real_)) %>%
    #then pivot that dtaframe longer 
    pivot_longer(-.id,
                 values_to = 'beta_50',
                 names_to = 'name') %>%
    #remove any NA values for medians
    filter(!is.na(beta_50)) %>%
    #make the name of each variable make more sense 
    mutate(var = str_sub(name, 2, length(name))) %>%
    #get the names to extend beyond the first case of 
    #each variable for categoricals
    mutate(.id = case_when(name > 'V1' ~ paste0(.id, 
                                                sep = "_", 
                                                var),
                           TRUE ~ .id)) %>%
    #rename generic column name
    dplyr::rename(parameter = `.id`)
  
  #redo whole process for the loewr and upper CI
  lower <- mcmc$q2.5
  lower <- lower[names(lower) %in% params] 
  
  lower1 <- rbindlist(
    lapply(lower, function(x) data.table(t(x))),
    fill = TRUE, idcol = TRUE
  ) %>%
    mutate(V1 = case_when(is.na(V2) ~ V1,
                          !is.na(V2) ~ NA_real_)) %>%
    pivot_longer(-.id,
                 values_to = 'beta_lower',
                 names_to = 'name') %>%
    filter(!is.na(beta_lower)) %>%
    mutate(var = str_sub(name, 2, length(name))) %>%
    mutate(.id = case_when(name > 'V1' ~ paste0(.id, 
                                                sep = "_", 
                                                var),
                           TRUE ~ .id)) %>%
    dplyr::rename(parameter = `.id`)
  
  upper <- mcmc$q97.5
  upper <- upper[names(upper) %in% params] 
  
  upper1 <- rbindlist(
    lapply(upper, function(x) data.table(t(x))),
    fill = TRUE, idcol = TRUE
  ) %>%
    mutate(V1 = case_when(is.na(V2) ~ V1,
                          !is.na(V2) ~ NA_real_)) %>%
    pivot_longer(-.id,
                 values_to = 'beta_upper',
                 names_to = 'name') %>%
    filter(!is.na(beta_upper)) %>%
    mutate(var = str_sub(name, 2, length(name))) %>%
    mutate(.id = case_when(name > 'V1' ~ paste0(.id, 
                                                sep = "_", 
                                                var),
                           TRUE ~ .id)) %>%
    dplyr::rename(parameter = `.id`)
  
  #join these three dataframes together by their
  #parameter names
  cont_vars <- median1 %>%
    left_join(lower1, by = c("parameter", "name", "var")) %>%
    left_join(upper1, by = c("parameter", "name", "var")) %>%
    dplyr::select(-name, -var)
  
  #function returns this dataframe
  return(cont_vars)
  
}

# Making a z and p-value df per parameter in a model ----------------------

z_pvalue <- function(x, name, number){
  df <- as.data.frame(x) %>%
    mutate(parameter = paste(name, 1:n(), sep = "_")) %>%
    rename("z" = 1) %>% 
    filter(!is.na(z)) %>%
    mutate(p = case_when(z >= 0.5 ~ (1-z), #I Think these are 1-tailed p-values but check with Kiona
                         z < 0.5 ~ (1 - (1-z))))
  
  return(df)
}

#END SCRIPT


# Load packages -----------------------------------------------------------


package.list <- c("tidyverse", 
                  "readxl", 'data.table')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_bw())

# Plot all variables in the model -----------------------------------------

#takes arguments of an mcmc object and the parameters
# you want to plot
all_estimates <- function(mcmc, 
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
    left_join(lower1, by = "parameter") %>%
    left_join(upper1, by = "parameter")
  
  #plot these with a their median and CI and 
  # a dashed line at 0 to show who does/doesn't cross zero
  plot <- ggplot(cont_vars, aes(x = reorder(parameter, -beta_50), y = beta_50)) +
    geom_point(size = 2) +
    geom_hline(yintercept= 0, linetype = 2) +
    geom_errorbar(aes(ymin = beta_lower, 
                      ymax = beta_upper), width = .2) +
    labs(y = "median", x = "parameter") +
    coord_flip()
  
  #the output is a list of this plot and of the dataframe,
  # in case the goal is to plot right away or if it is
  # to manipulate the dataframe any further for other purposes
  # (a table, color-coding inclusion/exclusion, etc.)
  output <- list(plot, cont_vars)
  
  return(output)
  
}

# Graph RHat per parameter ------------------------------------------------

rhat_graph_fun <- function(list){
  
  #this creates a dtaaframe out of the Rhat values from the model
  df <- data.frame(id = names(list),
                   Rhat = unlist(lapply(list, paste, collapse = ","))) %>%
    #splits Rhat by , when that list element had more than one value
    mutate(Rhat = str_split(Rhat, ",")) %>%
    #unnests - so makes each Rhat a new row in the df
    unnest(c(Rhat)) %>%
    #make sure Rhat is a numeric
    mutate(Rhat = as.numeric(Rhat)) 
  
  #plot histogram and make sure all below 1.1
  plot <- ggplot(df, aes(x = Rhat)) +
    geom_histogram() +
    geom_vline(xintercept = 1.1, linetype = 2) +
    theme_bw() +
    scale_y_sqrt() +
    #this is facetted by the parameter ID so you
    # can find problematic parameters
    facet_wrap(~ id)
  
  return(plot)
}


# Gelman for total model --------------------------------------------------

#this function plots one histogram overall for the whole model
# to diagnose gelman-rubin stats
rhat_graph_fun2 <- function(list){
  
  #this creates a dtaaframe out of the Rhat values from the model
  df <- data.frame(id = names(list),
                   Rhat = unlist(lapply(list, paste, collapse = ","))) %>%
    #splits Rhat by , when that list element had more than one value
    mutate(Rhat = str_split(Rhat, ",")) %>%
    #unnests - so makes each Rhat a new row in the df
    unnest(c(Rhat)) %>%
    #make sure Rhat is a numeric
    mutate(Rhat = as.numeric(Rhat)) 
  
  
  #plot histogram and make sure all below 1.1
  plot <- ggplot(df, aes(x = Rhat)) +
    geom_histogram() +
    geom_vline(xintercept = 1.1, linetype = 2) +
    theme_bw() +
    scale_y_sqrt() 
  
  return(plot)
}


# Predicted response graph function ---------------------------------------

# this creates a plot of the predicted response of a variable
# by transforming it from the logit scale (for binomial models)
# and then generating a predicted plot of median and 95%CI 
# across the entire range of the original variable in the model

# this function takes the beta estimates (all simulateions),
# the b0 estimates (all simulations), a transformed covariate variable
# to the scale of the model, a dataframe with both the transformed
# and untransformed covariate values, and the untransformed varaible
# as arguments

transformed_betas <- function(beta, b0, varS, metadf, var, color){
  
  #get the covariate response by multiplying the beta values by
  # the covariate that has been scaled to the data put
  # into the model
  #beta is a vector of the estimate for every simulation,
  #multiplied by the varS, which has a fixed length, creates
  # a matrix that has rows for simulations and columns for
  # each value in the finite scale of the scaled covariate
  response <- (beta %*% t(varS))
  
  #this just adds the b0 to correct for the intercept estimate
  logit_response <- b0 + response
  
  #then transform this by taking the plogis() of the logit repsonse
  transformed <- plogis(logit_response)
  
  #turn this into a datafram with long format such that each "level"
  # of the transformed covariate gets a row for each iteration
  df <- transformed %>%
    as.data.frame() %>%
    #"level" referrs to the transformed varS "levels" that a
    # are on a pre-defined finite number (I usually set to 20)
    pivot_longer(cols = c("V1":"V20"),
                 names_to = "level",
                 values_to = "p") %>%
    #clean up the level name
    mutate(level = str_sub(level, 2, length(level))) %>%
    #make numeric to bind with the metadata dataframe
    mutate(level = as.numeric(level)) %>%
    #left join to the untransformed values for the 
    # variable
    left_join(metadf, by = "level")
  
  #Summarise this dataframe to get an estimate for each
  # value along the axis of the covariate value a median
  # and upper and loewr 95% CI estimates to plot
  df_sum <- df %>%
    group_by(.data[[var]]) %>%
    summarise(median.p = median(p),
              LCI = quantile(p, prob = 0.025, type = 8),
              UCI = quantile(p, prob = 0.975, type = 8))
  
  #plot this with a nice ribbon for the CI and a line for the
  #median
  plot <- ggplot(df_sum, aes(x = .data[[var]], y = median.p)) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), fill = color, alpha = 0.2) +
    geom_line(size = 1, color = color) 
  
  return(plot)
  
}


# Logistic predicted graph ------------------------------------------------

logistic_predicted <- function(sum_mod, beta, metadf, varS, var, color){
  
  #get a vector of all the beta samples from the model
  beta_vec <- beta
  
  #pull out the median b0 estimate from teh model
  b0 <- as.data.frame(sum_mod$quantiles) %>%
    rownames_to_column(var = "parameter") %>%
    filter(parameter == "b0") %>%
    dplyr::select(`50%`) %>%
    as_vector()
  
  #multiply the betas across the transformed "meta" dataset variable
  # so that you have a matrix of beta estimates for a set of continuous
  # levels of the predictor variable
  t <- beta_vec %*% t(metadf$varS)
  
  #add in the b0
  logistic_response <- b0 + t
  
  #transform the response
  #response <- plogis(logistic_response)
  
  #add the metadata, make the dataframe tidy for summarising
  response_df <- as.data.frame(logistic_response) %>%
    #pivot the breaks in the continuous predictor longer
    pivot_longer(cols = c(V1:V20),
                 names_to = "level",
                 values_to = "estimate") %>%
    #get the levels to match the metadata file in name and type
    mutate(level = str_sub(level, start = 2, str_length(level))) %>%
    mutate(level = as.numeric(level)) %>%
    #join to metadata
    left_join(metadf, by = "level") 
  
  sum_df <- response_df %>%
    #group by the predictor variable levels
    group_by(level, .data[[var]]) %>%
    #get median and CI values
    summarise(median = plogis(median(estimate)),
              LCI = plogis(quantile(estimate, prob = 0.025, type = 8)),
              UCI = plogis(quantile(estimate, prob = 0.975, type = 8)))
  
  #plot this with a nice ribbon for the CI and a line for the
  #median
  plot <- ggplot(sum_df, aes(x = .data[[var]], y = median)) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2,
                fill = color) +
    geom_line(size = 1, color = color) 
  
  #function returns this plot
  return(plot)
  
  
}


# Logistic predicted for quadratic variable -------------------------------

# Logistic predicted graph ------------------------------------------------

logistic2_predicted <- function(sum_mod, beta, beta2, metadf, varS, var, color){
  
  #get a vector of all the beta samples from the model
  beta_vec <- beta
  beta2_vec <- beta2
  
  #pull out the median b0 estimate from teh model
  b0 <- as.data.frame(sum_mod$quantiles) %>%
    rownames_to_column(var = "parameter") %>%
    filter(parameter == "b0") %>%
    dplyr::select(`50%`) %>%
    as_vector()
  
  #multiply the betas across the transformed "meta" dataset variable
  # so that you have a matrix of beta estimates for a set of continuous
  # levels of the predictor variable
  t <- beta_vec %*% t(metadf$varS)
  t2 <- beta2_vec %*% t(metadf$varS^2)
  
  #add in the b0
  logistic_response <- b0 + t + t2
  
  #transform the response
  response <- plogis(logistic_response)
  
  #add the metadata, make the dataframe tidy for summarising
  response_df <- as.data.frame(response) %>%
    #pivot the breaks in the continuous predictor longer
    pivot_longer(cols = c(V1:V20),
                 names_to = "level",
                 values_to = "estimate") %>%
    #get the levels to match the metadata file in name and type
    mutate(level = str_sub(level, start = 2, str_length(level))) %>%
    mutate(level = as.numeric(level)) %>%
    #join to metadata
    left_join(metadf, by = "level") 
  
  sum_df <- response_df %>%
    #group by the predictor variable levels
    group_by(level, .data[[var]]) %>%
    #get median and CI values
    summarise(median = median(estimate),
              LCI = quantile(estimate, prob = 0.025, type = 8),
              UCI = quantile(estimate, prob = 0.975, type = 8))
  
  #plot this with a nice ribbon for the CI and a line for the
  #median
  plot <- ggplot(sum_df, aes(x = .data[[var]], y = median)) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2,
                fill = color) +
    geom_line(size = 1, color = color) 
  
  #function returns this plot
  return(plot)
  
  
}
# Log function transformation ---------------------------------------------

# this is the same function as the one above, but for log-transformed
# data types, such as poisson data

transformed_betas_2 <- function(beta, b0, varS, metadf, var){
  log_response <- b0 + (beta %*% t(varS))
  
  transformed <- exp(log_response)
  
  df <- transformed %>%
    as.data.frame() %>%
    pivot_longer(cols = c("V1":"V20"),
                 names_to = "level",
                 values_to = "p") %>%
    dplyr::mutate(level = str_sub(level, 2, length(level))) %>%
    dplyr::mutate(level = as.numeric(level)) %>%
    dplyr::left_join(metadf, by = "level")
  
  df_sum <- df %>%
    dplyr::group_by(.data[[var]]) %>%
    dplyr::summarise(median.p = median(p, na.rm = T),
              LCI = quantile(p, prob = 0.025, type = 8, na.rm = T),
              UCI = quantile(p, prob = 0.975, type = 8, na.rm = T))
  
  plot <- ggplot(df_sum, aes(x = .data[[var]], y = median.p)) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2) +
    geom_line(size = 1) 
  
  return(plot)
  
}

# Normal  -----------------------------------------------------------------

# this is the same function as the one above, but for lognormal data
lognormal_predicted <- function(mod, beta, metadf, varS, var, color){
  
  beta_vec <- beta
  
   b0 <- as.data.frame(mod$quantiles) %>%
     rownames_to_column(var = "parameter") %>%
     filter(parameter == "b0") %>%
     dplyr::select(`50%`) %>%
     as_vector()
  
   t <- beta_vec %*% t(metadf$varS)
   
   log_response <- b0 + t
   
   response_df <- as.data.frame(log_response) %>%
     pivot_longer(cols = c(V1:V20),
                  names_to = "level",
                  values_to = "estimate") %>%
     mutate(level = str_sub(level, start = 2, str_length(level))) %>%
     mutate(level = as.numeric(level)) %>%
     left_join(metadf, by = "level") 
   
    sum_df <- response_df %>%
      group_by(level, .data[[var]]) %>%
      summarise(median = median(estimate),
                LCI = quantile(estimate, prob = 0.025, type = 8),
                UCI = quantile(estimate, prob = 0.975, type = 8))

    
      plot <- ggplot(sum_df, aes(x = .data[[var]], y = exp(median))) +
        #geom_ribbon(aes(ymin = exp(LCI), ymax = exp(UCI)), alpha = 0.5,
        #            fill = color) +
        geom_line(size = 1.15, color = "black") +
        geom_line(size = 1, color = color) +
        labs(y = "Nest initation day (Julian date)") +
        theme(axis.title.x = element_blank())
      
      return(plot)
}

# Density of points to color ----------------------------------------------
#function is from https://slowkow.com/notes/ggplot2-color-by-density/
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}



# Covariate variation histograms ------------------------------------------

covariate_variation_fn <- function(df, vars){
  df2 <- df %>%
    dplyr::select(Project_ID, all_of(vars)) %>%
    pivot_longer(first(vars):last(vars),
                 names_to = "variable",
                 values_to = "value") 
  
  plot <- ggplot(df2, aes(x =  value)) +
    geom_histogram() +
    facet_grid(Project_ID~variable, scales = "free")
    
  
  return(plot)
}



# END SCRIPT




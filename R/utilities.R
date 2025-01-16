##### 0. Libraries ####
library(dplyr)
library(lubridate)
library(forecast)
library(mclust)
library(purrr)
library(furrr)
library(mgcv)
library(qgam)

#### 1. Data Preparation ####

sessions <- function(df,train_starts,train_ends,tz){
    res <- df %>%
      mutate(Start = floor_date(parse_date_time(Start,orders="%Y-%m-%d %H:%M:%S",tz=tz),"1 min"), 
             Start_Date = date(Start)) %>%
        filter(Start_Date <= train_ends,
              Start_Date >= train_starts) %>%
      group_by(Start_Date) %>%
      summarise(sessions = n()) %>%
      ungroup(.) %>%
      arrange(Start_Date) %>%
      mutate(weekend=ifelse(wday(Start_Date) %in% c(7,1),1,0))
    return(res)
}

transactions <- function(df,train_starts,train_ends,tz){
    res <- df %>%
      mutate(Start = floor_date(parse_date_time(Start,orders="%Y-%m-%d %H:%M:%S",tz=tz),"1 min"),
             Start_Date = date(Start)) %>%
        filter(Start_Date <= train_ends,
              Start_Date >= train_starts) %>%
      arrange(Start) %>%
      mutate(dow=wday(Start_Date)) %>%
    select(Start_Date,Arrival,Charge.Duration,Energy,dow)
    return(res)
}

#### 2. Fit ####

# gmm_fit <- function(df){
#     gmm_models <- list()
#     for (i in 1:7){
#         print(paste0("==== Iteration ",i,"/7 ===="))
#         temp <- df %>% filter(dow == i) %>% select(-dow)
#         gmm_models[[i]] <- Mclust(data = temp, G=2:30) 
#     }    
#     return(gmm_models)
# }

gmm_fit <- function(df, max_sample = NULL, verbose = TRUE) {
    start <- Sys.time()
    
    # Split data by 'dow'
    df_split <- split(df %>% select(Arrival,Charge.Duration,Energy), df$dow)
    
    # Use parallel processing for each group
    plan(multisession) # Use all available cores
    
    # Ensure parallel-safe RNG
    if(is.null(max_sample)){
            gmm_models <- future_map(df_split, ~ {
        sampled_data <- sample_n(., nrow(.)) 
        Mclust(data = sampled_data, G = 2:30) # Fit GMM
    }, .options = furrr_options(seed = TRUE)) 
    }else{
            gmm_models <- future_map(df_split, ~ {
        sampled_data <- sample_n(., min(max_sample, nrow(.))) 
        Mclust(data = sampled_data, G = 2:30) # Fit GMM
    }, .options = furrr_options(seed = TRUE)) 
    }

    if (verbose) {
        print(Sys.time() - start)
    }
    plan(sequential)
    return(gmm_models)
}

direct_model_fit <- function(train_dataframe, method, use_weather = FALSE){
    if(method == 'GAMlss'){
        if(use_weather){
            formulas <- list(
                Power ~ s(Tod, by=WeekDay, bs='cc', k=20) + WeekDay + Winter + Trend + 
                s(temperature98, k=10) + s(wind, k=10) + s(humidity,k=5),
                ~ s(Tod, by=WeekDay, bs='cc', k=20) + WeekDay + Winter + s(temperature) + s(humidity) + s(wind))
        } else {
            formulas <- list(
                Power ~ s(Tod, by=WeekDay, bs='cc', k=20) + WeekDay + Winter + Trend,
                ~ s(Tod, by=WeekDay, k=20) + WeekDay +  Winter)
        }
        model <- gam(formulas, family = gaulss, data = train_dataframe)
    }
    else if(method == 'QGAM'){
        if(use_weather){
            formula <- Power ~ s(Tod, by=WeekDay, bs='cc', k=20) + WeekDay + Winter + Trend + 
            s(temperature98, k=10) + s(wind, k=10) + s(humidity,k=5)
        } else{
            formula <- Power ~ s(Tod, by=WeekDay, bs='cc', k=20) + WeekDay + Winter + Trend
        }
    
        plan(multisession) 
        q <- seq(0.1, 0.9, 0.1)
        model <- future_map(q, function(x) return(qgam(formula, data = train_dataframe, qu=x)),
                            .options = furrr_options(seed = TRUE))
        plan(sequential)
    }
    else if(method == 'GAM'){
        if(use_weather){
            formula <- Power ~ s(Tod, by=WeekDay, bs='cc', k=20) + WeekDay + Winter + Trend + 
            s(temperature98, k=10) + s(wind, k=10) + s(humidity,k=5)
        } else{
            formula <- Power ~ s(Tod, by=WeekDay, bs='cc', k=20) + WeekDay + Winter + Trend
        }
        model <- gam(formula, data = train_dataframe)
    }
    else {
        stop(paste0('Method ', method, ' not available !'))
    }
    return(model)
}
#### 3. Predict ####

# SARIMA

sarima_simulate <- function(fit,sess,horizon,n_sim,verbose=TRUE){
        print("===== SARIMA simulation =====")
        start <- Sys.time()
    
X <- ts(sess$sessions,frequency=7)
probs <- seq(0.1,0.9,0.1)
forecast_dates = tibble(Start_Date=seq(from=tail(sess,1)$Start_Date+1, length.out=horizon, by="days")) %>%
      mutate(weekday = wday(Start_Date),
             weekend=ifelse(weekday %in% c(7,1),1,0))
x_reg <- forecast_dates %>% pull(weekend)

sim_results <- matrix(NA, nrow = n_sim, ncol = horizon)

# Perform simulations
for (i in 1:n_sim) {
  simulated_series <- numeric(horizon) # Placeholder for one simulation
  last_value <- tail(X, 1)          # Start with the last observed value

  for (t in 1:horizon) {
    # Generate a one-step-ahead forecast
    sim <- simulate(fit, nsim = 1, xreg=x_reg[t], future = TRUE, bootstrap = TRUE)
    simulated_series[t] <- sim
    fit <- Arima(c(X, simulated_series[1:t]), model = fit, xreg=c(sess$weekend,x_reg[1:t]) ) # Refit model with simulated series
  }
  
  # Store the simulation
  sim_results[i, ] <- simulated_series
}

# Convert simulations into a data frame of quantiles
forecast_quantiles <- round(apply(sim_results, 2, quantile, probs = probs, type=1))

# Output the quantiles
quantile_forecasts <- data.frame(
    Start_Date = forecast_dates$Start_Date,
    weekday = forecast_dates$weekday,
    q10 = forecast_quantiles[1, ],
    q20 = forecast_quantiles[2, ],
    q30 = forecast_quantiles[3, ],
    q40 = forecast_quantiles[4, ],
    q50 = forecast_quantiles[5, ],
    q60 = forecast_quantiles[6, ],
    q70 = forecast_quantiles[7, ],
    q80 = forecast_quantiles[8, ],
    q90 = forecast_quantiles[9, ]
)

    if (verbose){
        print(Sys.time() - start)
    }

# Prepare simulation data
prepped_simulations <- bind_cols(forecast_dates %>% select(Start_Date, weekday), data.frame(t(round(sim_results))))

    return(list(prepped_simulations,quantile_forecasts))
}

# GMM
relevant_sim <- function(gmm_model,n_sample){
    trans_sim <- NULL
    j=0
    while (j<n_sample){
        temp <- data.frame(sim(gmm_model$modelName,gmm_model$parameters,n=n_sample-j)[, -1, drop=FALSE])
        colnames(temp) <- c('Arrival','Charge.Duration','Energy')
        trans_sim <- bind_rows(temp,trans_sim) 
        trans_sim <- trans_sim %>% 
        filter(Arrival >0,Arrival<24,Charge.Duration>0,Charge.Duration<60*24,Energy>0,Energy<100)
        j <- nrow(trans_sim)
    }
    # print(n_sample)
    # print(nrow(trans_sim))
    
    return(trans_sim)
}

gmm_simulate <- function(gmm,quantile_forecasts,n_simu,verbose=TRUE){
    
    print("===== GMM simulation =====")
    start <- Sys.time()
    
    quantiles <- colnames(quantile_forecasts)[3:ncol(quantile_forecasts)]
    gmm_sim <- vector("list", length(quantiles)) # Preallocate for quantile groups
    names(gmm_sim) <- quantiles

    # Loop through quantiles
    for (quantile_index in seq_along(quantiles)) {
        q <- quantiles[quantile_index]
        
        simu_list <- vector("list", n_simu) # Preallocate for simulations
        
        # Prepare the data for the entire horizon
        n_samples <- quantile_forecasts[[q]] # Extract samples for the current quantile
        gmm_indices <- quantile_forecasts$weekday # GMM indices
        start_dates <- quantile_forecasts$Start_Date # Start dates
        
        for (n in seq_len(n_simu)) {
            # Generate all time steps at once for this simulation
            temp_list <- vector("list", horizon) # Preallocate for all time steps
            
            for (t in seq_len(horizon)) {
                temp_list[[t]] <- relevant_sim(gmm[[gmm_indices[t]]], n_samples[t])
                temp_list[[t]]$Start_Date <- start_dates[t]
            }
            
            # Combine all time steps for this simulation
            simu_list[[n]] <- bind_rows(temp_list)
        }
        
        # Store the simulations for the current quantile
        gmm_sim[[quantile_index]] <- simu_list
    }
    
    if (verbose){
        print(Sys.time() - start)
    }

    return(gmm_sim)
}

# Direct models
direct_model_predict <- function(test_dataframe, method, model){
    if(method == 'GAMlss'){
        gamlss.predict <- predict(model, newdata=test_dataframe)
        gamlss.mean_predict <- gamlss.predict[,1]
        gamlss.var_predict <- 0.01 + exp(gamlss.predict[,2])

        res.test <- map(seq(0.1,0.9,0.1), function(x){
            test_dataframe[paste0('Quantile', x)] <- qnorm(p=x, mean=gamlss.mean_predict, sd = gamlss.var_predict)  
            return(test_dataframe %>% select(Start, Tod, Power, paste0('Quantile', x)))})
        results <- reduce(res.test, full_join, by=c('Start', 'Tod', 'Power')) %>% 
            mutate_at(vars(starts_with('Quantile')), function(x) (ifelse(x < 0, 0, x)))
        return(results)
    }
    else if(method == 'QGAM'){
        q = seq(0.1,0.9,0.1)
        for(i in 1:length(q)){
            test_dataframe[paste0('Quantile', q[i])] <- predict(model[[i]], newdata=test_dataframe)
        }
        return(test_dataframe %>% mutate_at(vars(starts_with('Quantile')), function(x) (ifelse(x < 0, 0, x))))
    }
    else if(method == 'GAM'){
        test_dataframe['Mean'] <- predict(model, newdata = test_dataframe)
        return(test_dataframe %>% mutate(Mean = ifelse(Mean < 0, 0, Mean)))
    }
    else {
        stop(paste0('Method ', method, ' not available !'))
    } 
}
### 4. Load profile ### 

load_profile <- function(transactions,step,tz="US/Pacific"){
    # Convention commençante
    transactions <- transactions %>%
    mutate(Start = parse_date_time(paste0(Start_Date,'_',floor(Arrival),'_' ,floor((Arrival - floor(Arrival))*60) ),orders='%Y-%m-%d_%H_%M',tz=tz),
          End = Start + 60*Charge.Duration)
    
    start_date <- floor_date(min(transactions$Start), unit='day')
    end_date <- floor_date(max(transactions$Start), unit = 'day') + ddays(1) + dminutes(-step)

    load <- tibble(Start = seq(start_date, end_date, by = paste0(step, ' min')),
                   Power = 0)

    transactions$start_index <- interval(start_date, floor_date(transactions$Start,unit = paste0(step, ' min'))) %/% dminutes(step)
    transactions$end_index <- interval(start_date, floor_date(transactions$End,unit = paste0(step, ' min'))) %/% dminutes(step)
    final_index <- interval(start_date, end_date) %/% dminutes(step) + 1

    for (t in 1:nrow(transactions)){
        load_indices <- transactions[t, 'start_index']:(min(final_index,transactions[t, 'end_index']))
        P <- 60*transactions[t, 'Energy']/transactions[t, 'Charge.Duration']
        load$Power[load_indices] <- load$Power[load_indices] + P
    }
    
    return(load)
}

add_variables_to_profile <- function(df, step){
  
  # Calendar variables
  df <- df %>% mutate(Hour = as.factor(hour(Start)),
                      Tod = interval(floor_date(Start, unit='day'), Start) %/% dminutes(step) + 1)
  
  df <- df %>% mutate(Day = as.factor(day(Start)),
                      Month = as.factor(month(Start)),
                      Winter = Month %in% c(10,11,12,1,2,3),
                      WeekDay = as.factor(wday(Start)))
  
  df <- df %>% mutate(IsWeekEnd = WeekDay %in% c(7, 1))
  
  df <- df %>% mutate(Posan = interval(floor_date(Start, unit='year'), Start) %/% ddays(1),
                      Trend = as.integer(date(Start)-min(date(Start))))
  
  # Lags
  len_tod <- unique(df$Tod) %>% length
  for (i in c(seq(1,7), 14)){
    df[[paste0('Power_J_', i)]] <- lag(df$Power, i*len_tod)
  }
  
  return (df)   
}

get_meteo_data <- function(meteo_file_path){
  meteo <- readRDS(meteo_file_path) %>% 
    rename(Start = valid,
           temperature = tmpf,
           feel.temp = feel,
           humidity = relh,
           precipitation = p01i,
           visibility = vsby,
           wind.direction = drct,
           wind = sknt) %>%
    mutate(
      temperature = (temperature - 32)*5/9,
      feel.temp = (feel.temp - 32)*5/9,
      wind = ifelse(wind > 30, NA, wind)
    ) %>% select(Start, temperature, wind, humidity)
  
  meteo_analysis <- meteo %>% select(Start, temperature, humidity, wind) %>% 
    group_by(Date = date(Start)) %>% summarize(Nbr = n(), pct_NA_temp = sum(is.na(temperature))/n(), 
                                               pct_NA_hum = sum(is.na(humidity))/n(), pct_NA_wind = sum(is.na(wind))/n())
  
  # pb_days <- meteo_analysis %>% filter(Nbr < 5 | pct_NA_temp > 0.3 | pct_NA_hum >= 0.5 | pct_NA_wind > 0.4) %>% pull(Date)
  pb_days <- c(ymd('2019-01-05'), ymd('2019-04-07'), ymd('2019-04-26'))# No meteo data
  
  return(list('meteo_data' = meteo,
              'problem_dates' = pb_days))
}

smooth_temperature <- function(v, alpha)
{
  ylam <- v * (1-alpha)
  ylam[1] <- v[1]
  as.vector(stats::filter(ylam, filter = alpha, method = "rec"))
}

add_meteo_data <- function(profile, meteo_data){
  meteo <- meteo_data$meteo
  pb_days <- meteo_data$problem_dates
  
  for(var in meteo %>% colnames){
    if(var != 'Start'){
      profile[var] <- approx(x= meteo$Start, y=meteo[[var]], xout = profile$Start)$y
    }
  }  
  profile <- profile %>% mutate(temperature98 = smooth_temperature(temperature, 0.98))
  profile <- profile %>% mutate_at(all_of(vars(starts_with('temperature'), 'humidity', 'wind')), 
                                   function(x) ifelse(date(.$Start) %in% pb_days, NA, x))
  
  return(profile)
}
### 5. Metrics ###

rmse <- function(y_true,y_pred){
    return( sqrt(mean((y_true-y_pred)^2)))
}

mape <- function(y_true,y_pred){
    return( round( 100*(mean(abs(y_true-y_pred)/abs(y_true))), 2))
}

mae <- function(y_true,y_pred){
    return( sqrt(mean(abs(y_true-y_pred))))
}

nmae <- function(y_true,y_pred){
    return( round(100*mae(y_true,y_pred)/mean(y_true),2))
}

#### END ####
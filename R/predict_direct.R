### 0. Input data

source('R/utilities.R')
library(readr)

args=commandArgs(TRUE)
refit_starts = as.Date(args[1])
refit_ends = as.Date(args[2])
horizon = as.numeric(args[3])
use_weather = as.logical(args[4])
step = 15

tz <- "US/Pacific"

df <- read.csv("Data/palo_alto.csv")

### 1. test data preprocessing 
test_trans <- transactions(df = df, train_starts = refit_ends + 1, train_ends = refit_ends + horizon, tz = tz) 
test_profile <- load_profile(test_trans,step,tz=tz)
if(use_weather){
    meteo_data <- get_meteo_data('Data/weather.rds')
    test_profile <- test_profile %>% add_meteo_data(meteo_data)  
}
test_dataframe <- test_profile %>% add_variables_to_profile(step = step)

### 2. forecasts 
weather_ <- ifelse(use_weather, 'with_weather_', 'without_weather_')
models <- readRDS(paste0('R/models/direct_models_', weather_, as.character(refit_ends),'.rds'))
 
results_gam <- direct_model_predict(test_dataframe, method = 'GAM', model = models[['GAM']]) %>% 
               select(Start, Mean) %>%
               rename(mean = Mean)
results_qgam <- direct_model_predict(test_dataframe, method = 'QGAM', model = models[['QGAM']]) %>% 
                select(Start, starts_with('Quantile')) %>% 
                rename_at(vars(starts_with('Quantile')), 
                function(x) return(paste0('q', parse_number(x)*100, '_qgam')))
results_gamlss <- direct_model_predict(test_dataframe, method = 'GAMlss', model = models[['GAMlss']]) %>% 
                  rename_at(vars(starts_with('Quantile')),
                   function(x) return(paste0('q', parse_number(x)*100, '_gamlss')))

### 3. Formatting results
final <- results_gam %>% full_join(results_qgam, by='Start') %>% full_join(results_gamlss, by='Start')
saveRDS(final, paste0('Outputs/quantile_direct_', weather_, 'forecast_',as.character(refit_ends),'.rds'))

### END



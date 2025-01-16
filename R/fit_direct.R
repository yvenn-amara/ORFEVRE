### 0. Input data

source('R/utilities.R')

args=commandArgs(TRUE)
train_starts = args[1]
train_ends = args[2]
use_weather = as.logical(args[3])
timezone = "US/Pacific"
step = 15

### 1. Data preparation 
df <- read.csv("Data/palo_alto.csv")

train_trans <- transactions(df = df, train_starts = train_starts, train_ends = train_ends, tz = timezone)
train_profile <- load_profile(train_trans, step, tz = timezone)
if(use_weather){
    meteo_data <- get_meteo_data('Data/weather.rds')
    train_profile <- train_profile %>% add_meteo_data(meteo_data)  
}
train_dataframe <- train_profile %>% add_variables_to_profile(step = step)

### 2. Apply models
models <- list()
for (method in c('GAMlss', 'QGAM', 'GAM')){
    models[[method]] = direct_model_fit(train_dataframe, method, use_weather=use_weather)
}

### 3. Saving models
weather_ = ifelse(use_weather, 'with_weather_', 'without_weather_')
saveRDS(models, paste0('R/models/direct_models_', weather_, as.character(train_ends),'.rds'))
### END
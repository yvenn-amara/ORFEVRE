### 0. Input data

source('R/utilities.R')

args=commandArgs(TRUE)
train_starts = args[1]
train_ends = args[2]
df <- read.csv("Data/palo_alto.csv")

### 1. Sarima

sess <- sessions(df = df,train_starts = train_starts,train_ends = train_ends,tz = "US/Pacific")
X <- ts(sess$sessions,frequency=7)
sarima <- auto.arima(X,xreg=sess %>% pull(weekend), allowdrift = FALSE)

### 2. GMM

trans <- transactions(df = df,train_starts = train_starts,train_ends = train_ends,tz = "US/Pacific")
gmm <- gmm_fit(trans,max_sample=NULL)

### 3. Saving models

models <- list()
models[[1]] <- sarima
models[[2]] <- gmm

saveRDS(models, paste0('R/models/models_',as.character(train_ends),'.rds'))

### END
### 0. Input data

source('R/utilities.R')

args=commandArgs(TRUE)
refit_starts = as.Date(args[1])
refit_ends = as.Date(args[2])
horizon = as.numeric(args[3])
step = 15

tz <- "US/Pacific"
n_sim_sarima <- 100 # number of sarima simulations 
n_sim_gmm <- 100 # number of sarima simulations 
df <- read.csv("Data/palo_alto.csv")
models <- readRDS(paste0('R/models/models_',as.character(refit_ends),'.rds'))
### 1. Sarima

sess <- sessions(df = df,train_starts = refit_starts,train_ends = refit_ends,tz = "US/Pacific")
sarima_sim <- sarima_simulate(fit = models[[1]], sess = sess, horizon = horizon, n_sim = n_sim_sarima)

simulations <- sarima_sim[[1]]
quantile_forecasts <- sarima_sim[[2]]

### 2. GMM
# gmm_sim <- gmm_simulate(gmm = models[[2]], quantile_forecasts = quantile_forecasts,n_simu = n_sim_gmm)
gmm_sim <- gmm_simulate(gmm = models[[2]], quantile_forecasts = simulations,n_simu = n_sim_gmm)

### 3. Profiles reconstruction
print("===== Profiles reconstruction =====")
time_start <- Sys.time()
plan(multisession)
profiles <- flatten(gmm_sim) %>%
  future_map_dfr(load_profile, step=step)
plan(sequential)
print(Sys.time() - time_start)

### 4. Load Quantiles
final <- profiles %>%
group_by(Start) %>%
summarise(
  mean = mean(Power),
  q10 = quantile(Power, 0.10, type=1),
  q20 = quantile(Power, 0.20, type=1),
  q30 = quantile(Power, 0.30, type=1),
  q40 = quantile(Power, 0.40, type=1),
  q50 = quantile(Power, 0.50, type=1),
  q60 = quantile(Power, 0.60, type=1),
  q70 = quantile(Power, 0.70, type=1),
  q80 = quantile(Power, 0.80, type=1),
  q90 = quantile(Power, 0.90, type=1)
)

### 3. Saving simulations
saveRDS(final, paste0('Outputs/quantile_forecast_',as.character(refit_ends),'.rds'))

### END
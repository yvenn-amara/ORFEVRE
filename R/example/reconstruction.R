source('R/utilities.R')
df <- read.csv("Data/palo_alto.csv")
step=15
begin = '2019-01-07' 
end = '2019-09-29'
tz = "US/Pacific"

trans <- transactions(df = df,train_starts = begin,train_ends = end,tz = tz)
profile <- load_profile(trans,step,tz=tz)
print(head(profile))
print(tail(profile))

### END
library(openair)

target_list <- c("temp", "RH", "ws", "u10")
target_names <- c("temperature", "RH (%)", "wind speed (m/s)", "wind")

for (i in 1:length(target_list)) {

d <- timeVariation(wrf, pollutant = target_list[i], group = "data_source", type = "campaign", ci = T, ylab = target_names[i])
print(d, subset = "hour")
}
#this will work for showing all models, averaged across all sites separately for each campaign 
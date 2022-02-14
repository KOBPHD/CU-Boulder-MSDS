library(tidyverse)

url_prefix = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
file_names = c(
  "time_series_covid19_confirmed_US.csv", 
  "time_series_covid19_confirmed_global.csv", 
  "time_series_covid19_deaths_US.csv", 
  "time_series_covid19_deaths_global.csv"
)
urls = str_c(url_prefix, file_names)

raw_c_US = read_csv(urls[1], show_col_types = FALSE)
# raw_c_global = read_csv(urls[2], show_col_types = FALSE)
raw_d_US = read_csv(urls[3], show_col_types = FALSE)
# raw_d_global = read_csv(urls[4], show_col_types = FALSE)

c_US = raw_c_US
d_US = raw_d_US

#Drop areas that aren't states [EXPLAIN THIS]
drop_states = c(
  "American Samoa", 
  "Diamond Princess", 
  "District of Columbia", 
  "Grand Princess", 
  "Guam",
  "Northern Mariana Islands",
  "Puerto Rico",
  "Virgin Islands")

c_US = subset(c_US, !(Province_State %in% drop_states))
d_US = subset(d_US, !(Province_State %in% drop_states))

analysis_day = "2/12/22"

c_US = c_US[c(seq(1:11),which(names(c_US) == analysis_day))]
d_US = d_US[c(seq(1:12),which(names(d_US) == analysis_day))]

identical(c_US$Combined_Key, d_US$Combined_Key)

# long_deaths = data.frame(d_US$Long_, d_US[,analysis_day]/d_US$Population, d_US$Population, d_US[,analysis_day]/c_US[,analysis_day])
# names(long_deaths) = c("Longitude", "Deaths_Per_Capita", "Population", "Deaths_Per_Case)
# long_deaths = na.omit(long_deaths)
# long_deaths = long_deaths[is.finite(rowSums(long_deaths)),]
# plot(long_deaths)

lat_deaths = data.frame(d_US$Lat, d_US[,analysis_day]/d_US$Population, d_US$Population, d_US[,analysis_day]/c_US[,analysis_day])
names(lat_deaths) = c("Latitude", "Deaths_Per_Capita", "Population", "Deaths_Per_Case")
lat_deaths = na.omit(lat_deaths)
lat_deaths = lat_deaths[is.finite(rowSums(lat_deaths)),]
plot(
  x = lat_deaths$Latitude,
  y = lat_deaths$Deaths_Per_Capita,
  xlab = "Latitude (Degrees)",
  ylab = "COVID Deaths Per Capita",
  main = "Latitude vs. COVID Deaths Per Capita by County")
plot(
  x = lat_deaths$Latitude,
  y = lat_deaths$Deaths_Per_Case,
  xlab = "Latitude (Degrees)",
  ylab = "COVID Deaths Per Case",
  main = "Latitude vs. COVID Deaths Per Capita by County")

fit1 = lm(Deaths_Per_Capita~Latitude+Population, data = lat_deaths)
summary(fit1)
fit2 = lm(Deaths_Per_Case~Latitude+Population, data = lat_deaths)
summary(fit2)



# cor.test(long_deaths$Longitude, long_deaths$Deaths_Per_Capita)
# cor.test(lat_deaths$Latitude, lat_deaths$Deaths_Per_Capita)
# 
# lm(long_deaths$Deaths_Per_Capita~long_deaths$Longitude)
# lm(lat_deaths$Latitude, lat_deaths$Deaths_Per_Capita)


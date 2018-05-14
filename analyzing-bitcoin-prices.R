library(dplyr)
library(Quandl)
devtools::install_github('diplodata/gtrendsR')
library(gtrendsR)
library(wikipediatrend)
library(ggplot2)
library(lubridate)
library(gridExtra)
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
library(prophet)

##########
# GET DATA
##########

# get price data from Quandl 
df.bitcoin <- Quandl("BCHARTS/BITSTAMPUSD") %>%
  tbl_df()
# inspect
df.bitcoin
# add column mutating daily data to monthy, for combining this df with the Google Trends data (monthly)
df.bitcoin <- df.bitcoin %>%
  mutate(month = floor_date(Date, "month"))

# get Google Trend data
l.gtrend <- gtrends("bitcoin", time = "all")
# extract first df from list
df.interest_over_time <- l.gtrend[1] %>%
  as.data.frame() %>%
  tbl_df() 
# inspect
df.interest_over_time
# retain only first 2 cols
df.interest_over_time <- df.interest_over_time[, 1:2]
# rename cols
colnames(df.interest_over_time) <- c("date", "hits")
# inspect
df.interest_over_time
# replace "<1" with 0
df.interest_over_time <- df.interest_over_time %>%
  mutate(hits = if_else(hits == "<1", "0", hits)) %>%
  mutate_if(is.character, as.numeric) # convert chr vector to numeric

# get Wikipedia Trend
df.wiki_trend <- wp_trend("Bitcoin", from = "2016-01-01")
?wp_trend


# join datasets
df.bc <- left_join(df.bitcoin, df.interest_over_time, by = c("month" = "date"))

################
# VISUALIZE DATA
################

# daily price over time
d <- ggplot(data = df.bc, aes(x = Date, y = `Weighted Price`))  +
  geom_bar(stat = "identity") +
  geom_line() 

# monthly web hits over time
m <- ggplot(data = df.bc, aes(x = month, y = hits))  +
  geom_line()

# combine charts
grid.arrange(d, m, nrow = 2)


###################
# LINEAR REGRESSION
###################

# get correlation between price and google hits
cor.test(df.bc$hits, df.bc$`Weighted Price`) #0.9590221

# build model
model <- lm(`Weighted Price` ~ hits, data = df.bc)
summary(model)

###################
# ANOMALY DETECTION
###################

# create 2 column data.frame, with the first column being a set of timestamps, and the second coloumn being numeric values.
df.bc_anomaly <- df.bitcoin %>%
  select(Date, `Weighted Price`) %>%
  mutate_if(is.Date, as.POSIXct)
# inspect 
df.bc_anomaly

# detect anomalies
AnomalyDetectionTs(df.bc_anomaly,max_anoms=0.02, direction='both', plot=TRUE, y_log = TRUE)
?AnomalyDetectionTs
data(raw_data)

###################
# PROPHET ALGORITHM
###################

df.bc_anomaly <- df.bitcoin %>%
  select(Date, `Weighted Price`) %>%
  mutate_if(is.Date, as.POSIXct)

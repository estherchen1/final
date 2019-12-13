```{r daily info}
#taking the average of each variable by date
daily_mean_temp <- df %>% group_by(date) %>% summarize(temp = mean(temp))
daily_mean_vol <- traffic %>% group_by(date) %>% summarize(vol = mean(traffic_volume))
daily_mean_rain <- traffic %>% group_by(date) %>% summarize(rain = mean(rain_1h))
daily_mean_snow <- traffic %>% group_by(date) %>% summarize(snow = mean(snow_1h))
daily_mean_cloud <- traffic %>% group_by(date) %>% summarize(cloud = mean(clouds_all))
is_holiday <- df %>% group_by(date) %>% summarize(holiday = mean(holiday))
```

```{r weather}
x <- traffic %>% group_by(date) %>% select(weather_main)

daily_weather <- x %>% group_by(date) %>% summarize(weather = calculate_mode(weather_main))
```

```{r average df}
avg <- merge(daily_weather, merge(is_holiday, merge(daily_mean_cloud, merge(daily_mean_snow, merge(daily_mean_rain,merge(daily_mean_temp, daily_mean_vol))))))
```

```{r avg format}
avg %>% 
  mutate(
    holiday = ifelse(holiday != 0, 1, 0),
  )
```
```{r sample}
sample <- sample_n(df, 1000, replace = FALSE)
```

```{r}
unique(dff$year)
```
```{r data process for nn}
#collapse some of the variables actually further...skdjfljfl
dff <- df[, !(colnames(df) %in% c("weather_description","hour","traffic_volume","month","date_time"))]
dmy <- dummyVars(" ~ .", data = dff, fullRank = TRUE)
dmydf <- as.data.frame(predict(dmy, newdata=dff))
```

```{r}
df1 <- df[, colnames(df) %in% c("traffic_volume")]
```

```{r}

```

```{r}
nndat <- cbind(df1, dmydf)
```

```{r}
colnames(trainNN)
```

```{r test train}
samplesize = 0.60 * nrow(nndat)
set.seed(1)
index = sample( seq_len ( nrow ( nndat ) ), size = samplesize )

datatrain = nndat[ index, ]
datatest = nndat[-index, ]
```

```{r scale data}
max = apply(nndat , 2 , max)
min = apply(nndat, 2 , min)
scaled = as.data.frame(scale(nndat, center = min, scale = max - min))
```

```{r}
# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]
```
```{r}
# fit neural network
set.seed(2)
NN = neuralnet(traffic_volume ~ temp, data = trainNN)
```
```{r}
# plot neural network
plot(NN)
```

```{r arima time series????}
voltime <- seventeen %>% filter(season == "Fall") %>% select(date_time, traffic_volume)
volts <- ts(voltime)
```

```{r}
autoplot(volts)
```
```{r testing and training}
data_train <- DT[date %in% n_date[43:63]]
data_test <- DT[date %in% n_date[64]]
```
```{r}
averages <- data.table(traffic_volume = rep(sapply(0:2, function(i)
  mean(data_train[((i*period*7)+1):((i+1)*period*7), traffic_volume])),
  each = period * 7),
  date_time = data_train$date_time)

ggplot(data_train, aes(date_time, traffic_volume)) +
  geom_line() +
  geom_line(data = averages, aes(date_time, traffic_volume),
            linetype = 5, alpha = 0.75, size = 1.2, color = "firebrick2") +
  labs(x = "Date", y = "Traffic Volume") 
```

```{r}
data_ts <- ts(data_train$traffic_volume, freq = period * 7)
decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)$time.series

decomp_stl <- data.table(Load = c(data_train$traffic_volume, as.numeric(decomp_ts)),
                         Date = rep(data_train[,date_time], ncol(decomp_ts)+1),
                         Type = factor(rep(c("original data", colnames(decomp_ts)),
                                           each = nrow(decomp_ts)),
                                       levels = c("original data", colnames(decomp_ts))))

ggplot(decomp_stl, aes(x = Date, y = Load)) +
  geom_line() + 
  facet_grid(Type ~ ., scales = "free_y", switch = "y") +
  labs(x = "Date", y = NULL,
       title = "Time Series Decomposition by STL")
```


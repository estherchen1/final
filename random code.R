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



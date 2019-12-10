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
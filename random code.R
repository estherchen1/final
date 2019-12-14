
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



```{r traindata}
#taking out a year of data
#data_r <- traindata[(date %in% n_date[1:30])]


#N <- nrow(data_r) # number of observations in the train set

#window <- N / period # number of days in the train set

#matrix_ts <- data.table(Volume = data_r[, traffic_volume], 
#                         Daily = rep(1:period, window),
#                         Weekly = data_r[, day_num])
```

```{r}
N <- nrow(data_r)
window <- N / period # number of days in the train set
# 1, ..., period, 1, ..., period - and so on for the daily season 
# using feature "day_num" for the weekly season
matrix_train <- data.table(volume = data_r[, traffic_volume],
                           Daily = as.factor(rep(1:period, window)),
                           Weekly = as.factor(data_r[, day_num]))
```
```{r}
lm_m_1 <- lm(volume ~ 0 + ., data = matrix_train)
smmr_1 <- summary(lm_m_1)
paste("R-squared: ",
      round(smmr_1$r.squared, 3),
      ", p-value of F test: ",
      1-pf(smmr_1$fstatistic[1], smmr_1$fstatistic[2], smmr_1$fstatistic[3]))
```
```{r}
datas <- rbindlist(list(data_r[, .(traffic_volume, date_time)],
                        data.table(traffic_volume = lm_m_1$fitted.values, data_time = data_r[, date_time])))
datas[, type := rep(c("Real", "Fitted"), each = nrow(data_r))]

ggplot(data = datas, aes(date_time, traffic_volume, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Traffic Volume",
       title = "Fit from MLR")
```
```{r}
ggplot(data = data.table(Fitted_values = lm_m_1$fitted.values,
                         Residuals = lm_m_1$residuals),
       aes(Fitted_values, Residuals)) +
  geom_point(size = 1.7) +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  labs(title = "Fitted values vs Residuals")
```
```{r}
ggQQ <- function(lm){
  # extract standardized residuals from the fit
  d <- data.frame(std.resid = rstandard(lm))
  # calculate 1Q/4Q line
  y <- quantile(d$std.resid[!is.na(d$std.resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  p <- ggplot(data = d, aes(sample = std.resid)) +
    stat_qq(shape = 1, size = 3) +         # open circles
    labs(title = "Normal Q-Q",             # plot title
         x = "Theoretical Quantiles",      # x-axis label
         y = "Standardized Residuals") +   # y-axis label
    geom_abline(slope = slope, intercept = int, linetype = "dashed",
                size = 1, col = "firebrick1") # dashed reference line
  return(p)
}
```
```{r}
ggQQ(lm_m_1)
```
```{r}
lm_m_2 <- lm(volume ~ 0 + Daily + Weekly + Daily:Weekly,
             data = matrix_train)
c(Previous = summary(lm_m_1)$r.squared, New = summary(lm_m_2)$r.squared)
```
```{r}
datas <- rbindlist(list(data_r[, .(traffic_volume, date_time)],
                        data.table(traffic_volume = lm_m_2$fitted.values, data_time = data_r[, date_time])))
datas[, type := rep(c("Real", "Fitted"), each = nrow(data_r))]

ggplot(data = datas, aes(date_time, traffic_volume, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Traffic VOlume",
       title = "Fit from MLR")
```
```{r}
ggplot(data = data.table(Fitted_values = lm_m_2$fitted.values,
                         Residuals = lm_m_2$residuals),
       aes(Fitted_values, Residuals)) +
  geom_point(size = 1.7) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  labs(title = "Fitted values vs Residuals")
```


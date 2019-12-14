
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




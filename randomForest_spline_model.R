library("tidyverse")
library("caret")
library("naniar")
library("stringr")
library("lubridate")
library("earth")

#load the training data and test data set
train.total <- read.csv("./Desktop/DATA MINING/comp3/all/train_v2_flat.csv")
test <- read.csv("./Desktop/DATA MINING/comp3/all/test_v2_flat.csv", colClasses=c(fullVisitorId="character"))

#split training data into 0.7 training data set and 0.3 test set
trainIndex = createDataPartition(train.total$fullVisitorId, p=0.7, list=FALSE,times=1)

#creat training data set and test data set
train <- train.total[trainIndex,]
test.set <-train.total[-trainIndex,]
sample <- read.csv("./Desktop/DATA MINING/comp3/all/sample_submission_v2.csv",colClasses=c(fullVisitorId="character"))

#check if the test set is loaded correctly
sum(unique(test$fullVisitorId) %in% unique(sample$fullVisitorId))
train$log_revenue <- log(train$totals_transactionRevenue)
#check missing values for each feature
num_na <- data.frame("percent"=apply(is.na(train), 2, sum))
num_na$percent <- num_na$percent/nrow(train)
num_na
#data visulization
glimpse(train)
gg_miss_var(train)
#log transaction distribution
g <- ggplot(train, aes(x = log_revenue)) + geom_histogram(fill="green", color = "black")
g
#distribution(exclude 0)
g <- ggplot(train[train$log_revenue !=0,], aes(x = log_revenue)) + geom_histogram(fill="green", color = "black", bins =30)
g

#ismobile vs log_revenue by continent
g <- ggplot(train, aes(x = device_isMobile, y = log_revenue, color = device_isMobile )) + geom_boxplot() 
g + facet_grid(.~geoNetwork_continent)

#operating system vs log_transaction
g <- ggplot(train, aes(x = device_operatingSystem , y = log_revenue, color = device_operatingSystem )) + geom_boxplot() 
g +coord_flip()
#we can see that most revenues are generated from users using Windows

#pageviews vs log_revenue by medium
g <- ggplot(train, aes(x = log_revenue, y =totals_pageviews , color = trafficSource_medium)) + geom_point() +coord_flip()
g+ facet_grid(trafficSource_medium~.)

train$log_revenue[is.na(train$log_revenue)] <- 0
train$date<- ymd(train$date)
#daily log revenue vs date
d2 <- train %>% group_by(date) %>% summarise(log_Revenue = sum(log_revenue)) %>%
  ggplot(aes(x=date, y=log_Revenue)) + geom_line(col='blue') + geom_smooth(col='red') + scale_x_date(date_breaks = "1 month", date_labels = "%b %d")
d2

train$month <- month(train$date)
#month verse log_revenue
d3 <- train %>% group_by(month) %>% summarise(log_Revenue = sum(log_revenue)) %>%
  ggplot(aes(x=month, y=log_Revenue)) + geom_line(col='blue') + geom_smooth(col='red') + xlim(1, 12)
d3
d1 <- train %>% group_by(date) %>% summarise(dailySessions = n()) %>%
  ggplot(aes(x=date, y=dailySessions)) + geom_line(col='blue') + geom_smooth(col='red') +
  labs(x="", y="Sessions per Day") + scale_x_date(date_breaks = "1 month", date_labels = "%b %d")
d1
#convert data into week and day
train$weekday <- wday(train$date, label=TRUE)
#test$weekday <- wday(test$date, label=TRUE)
str(train$weekday)

plotRevenue <- function(dataframe, factorVariable, topN=10) {
  var_col <- enquo(factorVariable)
  dataframe %>% group_by(!!var_col) %>% summarize(rev=sum(log_revenue)) %>% filter(rev>0) %>% top_n(topN, wt=rev) %>% ungroup() %>%
    ggplot(aes_(x=var_col, y=~rev, fill=var_col)) +
    geom_bar(stat='identity')+
    labs(x="", y="Log_Revenues (USD)")+
    theme(legend.position="none")
}
#weekday vs  total revenue
w2 <- plotRevenue(train, weekday)
w2



#channelGrouping vs log_revenue
sessionOrder <- train %>% count(channelGrouping) %>% top_n(10, wt=n) %>% arrange(desc(n))
sessionOrder <- sessionOrder$channelGrouping
c2 <- plotRevenue(train, channelGrouping) + scale_x_discrete(limits=sessionOrder)
c2

train$totals_pageviews[is.na(train$totals_pageviews)] <- 0
uni.df <- data.frame(apply(train, 2, function(x) length(unique(x))))
colnames(uni.df) <- c("count")
uni.df

#find features with less missing values and relatively low variance(not too high or too low)
cols <- c("channelGrouping","totals_hits",'trafficSource_medium','device_deviceCategory','geoNetwork_continent','geoNetwork_subContinent','device_isMobile','totals_pageviews',"month",'log_revenue')

#=================
#10 fold cross validation
#=================
#randomly select 200000 records for cross validation
set.seed(33)
cols <- c("channelGrouping","totals_hits",'trafficSource_medium','device_deviceCategory','geoNetwork_continent','geoNetwork_subContinent','device_isMobile','totals_pageviews',"month","log_revenue")

#fill na values with0
train$totals_pageviews[is.na(train$totals_pageviews)] <- 0 

#randomly select 200000record for cross validation due to memory limitation
mysample <- train[sample(1:nrow(train), 200000, replace=FALSE),]
mysample <- mysample[,cols]
control <- trainControl(method="cv", number=10)
tunegrid <- expand.grid(.mtry=7)
#not able to try a great number of trees because of the memory limitation
rf_default <- train(log_revenue~., data=mysample,importance = TRUE, method="rf", metric="RMSE", ntree=5,tuneGrid=tunegrid, trControl=control)

#print out the results
print(rf_default)

#check the feature importance
varImp(rf_default)

#=================
#try different features based on the randomforest results
#=================
set.seed(33)
#select and ajust features 
cols <- c("channelGrouping","totals_hits",'trafficSource_medium','device_deviceCategory','geoNetwork_continent','geoNetwork_subContinent','totals_pageviews','log_revenue')
#randomly select 200000 records for cross validation
mysample <- train[sample(1:nrow(train), 200000, replace=FALSE),]
mysample <- mysample[,cols]
control <- trainControl(method="cv", number=10)
tunegrid <- expand.grid(.mtry=7)
#not able to try a great number of trees because of the memory limitation
rf_default <- train(log_revenue~., data=mysample,importance = TRUE, method="rf", metric="RMSE", ntree=5,tuneGrid=tunegrid, trControl=control)
#print out the rf result
print(rf_default)


#==============================
#test the model on 0.3 test set
#==============================
test.set <-train.total[-trainIndex,]
#data preprocessing
test.set$totals_transactionRevenue <- log(test.set$totals_transactionRevenue)

#fill na values with 0
test.set$totals_transactionRevenue[is.na(test.set$totals_transactionRevenue)] <- 0

#convert date into date format
test.set$date<- ymd(test.set$date)

#extract month from the date
test.set$month <- month(test.set$date)
#fill NA with 0
test.set$totals_pageviews[is.na(test.set$totals_pageviews)] <- 0
true.y <- test.set$totals_transactionRevenue
test.set <- test.set[,cols2]

#prediction
cv.pred <- predict(rf_default,test.set)
pred.df <- data.frame(prediction = cv.pred)
#calculate RMSE
sqrt(sum((pred.df - true.y)^2)/nrow(test.set))

#=================
#build model ---randomforest
#=================
set.seed(23)
total_train <- train[,cols]
#build a model on the whole training data set using 7 trees(not able to deal with a high number of trees due to memory limitation)
rf_default <- train(log_revenue~., data=total_train, method="rf",ntree=7, tuneGrid = data.frame(mtry=9))
 
#test data
#perform the same data preprocessing
cols2 <- c("channelGrouping","totals_hits",'trafficSource_medium','device_deviceCategory','geoNetwork_continent','geoNetwork_subContinent','device_isMobile','totals_pageviews',"month")
test$date<- ymd(test$date)
test$month <- month(test$date)

new_test <- test[,cols2]
fullid<- test[,c("fullVisitorId")]

#fill NA values with 0
new_test$totals_pageviews[is.na(new_test$totals_pageviews)] <- 0
pred <- predict(rf_default, new_test)

#convert it back to the original scale
new_pred <- exp(pred)
new_df <- data.frame(fullid,new_pred)

unique(new_df$fullVisitorId) 
#rename the columns
colnames(new_df) <- c("fullVisitorId","PredictedLogRevenue")

#aggregrate the total_revenue by fullvistorID
new_df2 <- aggregate(PredictedLogRevenue ~ fullVisitorId,new_df,sum )

#sum up total revenues for each person
sum(unique(new_df2$fullVisitorId) %in% sample$fullVisitorId)
new_df2$PredictedLogRevenue[new_df2$PredictedLogRevenue <=100] <-0
#apply log to each row
kog <- function(x) ifelse(x!=0, log(x), 0)
test$fullVisitorId
new_df2$PredictedLogRevenue <- lapply(new_df2$PredictedLogRevenue,kog)
#combind the prediction result into a new dataframe
a<- data.frame(unlist(new_df2$PredictedLogRevenue))
fina_df <- data.frame(cbind(new_df2$fullVisitorId,a))
colnames(fina_df) <- c("fullVisitorId","PredictedLogRevenue")

#output the final prediction
write.csv(fina_df,"./Desktop/DATA MINING/comp3/test_submisson3.csv", row.names = F)


#===================================
#spline model
#===================================
#==============================
#cross validation
#==================================

set.seed(33)
cols <- c("channelGrouping","totals_hits",'trafficSource_medium','device_deviceCategory','geoNetwork_continent','geoNetwork_subContinent','device_isMobile','totals_pageviews',"month","log_revenue")
train$totals_pageviews[is.na(train$totals_pageviews)] <- 0 

#randomly select 500000 records for cross validation
mysample <- train[sample(1:nrow(train), 500000, replace=FALSE),]
mysample <- mysample[,cols]

#create training set for cv
cv.ind <- sample(1:nrow(mysample), 250000, replace=FALSE)
cv.sample <- mysample[cv.ind,]
cv.test.sample <- mysample[-cv.ind,]

#build a spline model
earth_default <- earth(log_revenue~., data=cv.sample)
cv.predi <- predict(earth_default,cv.test.sample)
sqrt(sum((cv.predi - cv.test.sample$log_revenue)^2)/nrow(cv.test.sample))
#[1] 1.675687

print(earth_default)

#======================
#predict on the 0.3 test set
#======================

test.set <-train.total[-trainIndex,]
#data preprocessing
test.set$totals_transactionRevenue <- log(test.set$totals_transactionRevenue)
test.set$totals_transactionRevenue[is.na(test.set$totals_transactionRevenue)] <- 0
#convert date into date format
test.set$date<- ymd(test.set$date)

#extract month from the date
test.set$month <- month(test.set$date)

#fill missing values with 0
test.set$totals_pageviews[is.na(test.set$totals_pageviews)] <- 0
true.y <- test.set$totals_transactionRevenue
test.set <- test.set[,cols2]
#prediction
cv.pred <- predict(earth_default,test.set)
pred.df <- data.frame(prediction = cv.pred)
#calculate RMSE
sqrt(sum((pred.df - true.y)^2)/nrow(test.set))
#[1] 1.652634

#===================================
#use different features in spline models
#======================================
cols <- c("channelGrouping","totals_hits",'trafficSource_medium','device_deviceCategory','geoNetwork_continent','geoNetwork_subContinent','totals_pageviews','log_revenue')
#==============================
#cross validation
#==================================

set.seed(33)
#fill missing values with 0
train$totals_pageviews[is.na(train$totals_pageviews)] <- 0 

#randomly select 500000 records for cross validation
mysample <- train[sample(1:nrow(train), 500000, replace=FALSE),]
mysample <- mysample[,cols]

#create training set for cv
cv.ind <- sample(1:nrow(mysample), 250000, replace=FALSE)
cv.sample <- mysample[cv.ind,]
cv.test.sample <- mysample[-cv.ind,]
#build a spline model
earth_default <- earth(log_revenue~., data=cv.sample)

#prediction 
cv.predi <- predict(earth_default,cv.test.sample)
sqrt(sum((cv.predi - cv.test.sample$log_revenue)^2)/nrow(cv.test.sample))
#[1] 1.675687

#======================
#predict on the 0.3 test set
#======================
cols2 <- c("channelGrouping","totals_hits",'trafficSource_medium','device_deviceCategory','geoNetwork_continent','geoNetwork_subContinent','totals_pageviews')

test.set <-train.total[-trainIndex,]
#data preprocessing
test.set$totals_transactionRevenue <- log(test.set$totals_transactionRevenue)

#fill missing values with 0
test.set$totals_transactionRevenue[is.na(test.set$totals_transactionRevenue)] <- 0
#convert date into date format
test.set$date<- ymd(test.set$date)

#extract month from the date
test.set$month <- month(test.set$date)
test.set$totals_pageviews[is.na(test.set$totals_pageviews)] <- 0
true.y <- test.set$totals_transactionRevenue
test.set <- test.set[,cols2]

#prediction
cv.pred <- predict(earth_default,test.set)
pred.df <- data.frame(prediction = cv.pred)

#calculate RMSE
sqrt(sum((pred.df - true.y)^2)/nrow(test.set))
#[1] 1.652634

#======================
#predict on the real test set
#======================
cols2 <- c("channelGrouping","totals_hits",'trafficSource_medium','device_deviceCategory','geoNetwork_continent','geoNetwork_subContinent','device_isMobile','totals_pageviews',"month")
test$date<- ymd(test$date)
test$month <- month(test$date)

new_test <- test[,cols2]
fullid<- test[,c("fullVisitorId")]

#fill missing values with 0
new_test$totals_pageviews[is.na(new_test$totals_pageviews)] <- 0
pred <- predict(earth_default, new_test)

#convert it back to the original scale
new_pred <- exp(pred)
new_df <- data.frame(fullid,new_pred)

colnames(new_df) <- c("fullVisitorId","PredictedLogRevenue")

#aggregrate the total_revenue by fullvistorID
new_df2 <- aggregate(PredictedLogRevenue ~ fullVisitorId,new_df,sum )

#sum up total revenues for each person
#sum(unique(new_df2$fullVisitorId) %in% sample$fullVisitorId)
new_df2$PredictedLogRevenue[new_df2$PredictedLogRevenue <=100] <-0

#apply log to each row
kog <- function(x) ifelse(x!=0, log(x), 0)
new_df2$PredictedLogRevenue <- lapply(new_df2$PredictedLogRevenue,kog)

#convert prediction result into a new data frame
a<- data.frame(unlist(new_df2$PredictedLogRevenue))
fina_df <- data.frame(cbind(new_df2$fullVisitorId,a))
colnames(fina_df) <- c("fullVisitorId","PredictedLogRevenue")
#sub <- read.csv("./Desktop/DATA MINING/comp3/all/sub.csv")

#output the prediction into a csv file
write.csv(fina_df,"./Desktop/DATA MINING/comp3/spline_submission.csv", row.names = F)
sqrt(sum((fina_df$PredictedLogRevenue - sub$PredictedLogRevenue)^2)/nrow(fina_df))

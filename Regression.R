#SYS 6018: Applied Data Mining
#Competition 4: Google Analytics Customer Revenue Prediction

#Setting working directory to source file location
setwd("E:/Fall Term/SYS 6018 Applied Data Mining/Competitions/sys6018-competition-revenue-prediction/Code")
#Moving up one folder
setwd('..')

#Loading the required packages
library(readr)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(glmnet)
library(caret)
library(lubridate)
library(earth)

#Importing the training and the testing datasets
#Skipping the hits and customDimensions column to reduce size and load into R
ctypes = cols(fullVisitorId = col_character(),
              channelGrouping = col_character(),
              date = col_datetime(),
              device = col_character(),
              geoNetwork = col_character(),
              socialEngagementType = col_skip(), 
              totals = col_character(),
              trafficSource = col_character(),
              visitId = col_integer(), 
              visitNumber = col_integer(),
              visitStartTime = col_integer(),
              hits = col_skip(),
              customDimensions = col_skip())

train = read_csv("Data/train_v2.csv", col_types = ctypes)
test = read_csv("Data/test_v2.csv", col_types = ctypes)

#Parsing the JSON columns
tr_device <- paste("[", paste(train$device, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_geoNetwork <- paste("[", paste(train$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_totals <- paste("[", paste(train$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_trafficSource <- paste("[", paste(train$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)

te_device <- paste("[", paste(test$device, collapse = ","), "]") %>% fromJSON(flatten = T)
te_geoNetwork <- paste("[", paste(test$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
te_totals <- paste("[", paste(test$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
te_trafficSource <- paste("[", paste(test$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)

#Combine to make the full training and test sets
train <- train %>%
  cbind(tr_device, tr_geoNetwork, tr_totals, tr_trafficSource) %>%
  select(-device, -geoNetwork, -totals, -trafficSource)

test <- test %>%
  cbind(te_device, te_geoNetwork, te_totals, te_trafficSource) %>%
  select(-device, -geoNetwork, -totals, -trafficSource)

#Remove temporary tr_ and te_ sets
rm(tr_device); rm(tr_geoNetwork); rm(tr_totals); rm(tr_trafficSource)
rm(te_device); rm(te_geoNetwork); rm(te_totals); rm(te_trafficSource)

#Putting it in a new dataframe for later use
write.csv(train, "train_v2_flat.csv", row.names = F)
write.csv(test, "test_v2_flat.csv", row.names = F)

train = read_csv("Data/train_v2_flat.csv")
test = read_csv("Data/test_v2_flat.csv")

#Number of columns in the new training and test sets. 
ncol(train)
ncol(test)
#Extra column in train: campaignCode(1 non NA value)

#Since the campaignCode has just 1 non NA value, we remove this column from the train dataset
train = train %>% select(-one_of("campaignCode"))

#Getting the number of distinct/unique values in each of the columns in the train dataset
feat_uniq_values <- as.data.frame(sapply(train, n_distinct))
feat_uniq_values$column = row.names(feat_uniq_values)
names(feat_uniq_values) = c("uniqvalcount", "column")
row.names(feat_uniq_values) = NULL
feat_uniq_values = feat_uniq_values[,c(2,1)]

#Removing columns that have the same value throughout
feat_del <- feat_uniq_values$column[feat_uniq_values$uniqvalcount == 1]
train = train %>% select(-one_of(feat_del))
test = test %>% select(-one_of(feat_del))

#missing values in each column
#We need to take into account that such values as 
#“not available in demo dataset”, “(not set)”, “unknown.unknown”, “(not provided)” can be treated as NA
#Making these values as NA first
is_na_val <- function(x) x %in% c("not available in demo dataset", "(not provided)",
                                  "(not set)", "<NA>", "unknown.unknown",  "(none)")
train = train %>% mutate_all(funs(ifelse(is_na_val(.), NA, .)))
test = test %>% mutate_all(funs(ifelse(is_na_val(.), NA, .)))

#Getting the number of missing values in each column
na_values = as.data.frame(sapply(train, function(x)sum(is.na(x))))
na_values$column = row.names(na_values)
names(na_values) = c("missingvalcount", "column")
row.names(na_values) = NULL
na_values = na_values[,c(2,1)]

#Converting the logical variables to 0/1
train <- train %>% 
  mutate(isMobile = ifelse(isMobile, 1L, 0L),
         isTrueDirect = ifelse(isMobile, 1L, 0L),
         adwordsClickInfo.isVideoAd = ifelse(adwordsClickInfo.isVideoAd, 1L, 0L)) 

test <- test %>% 
  mutate(isMobile = ifelse(isMobile, 1L, 0L),
         isTrueDirect = ifelse(isMobile, 1L, 0L),
         adwordsClickInfo.isVideoAd = ifelse(adwordsClickInfo.isVideoAd, 1L, 0L)) 

#Replace NA values in revenue with 0
train$transactionRevenue[is.na(train$transactionRevenue)] <- 0
test$transactionRevenue[is.na(test$transactionRevenue)] <- 0

#EDA

#Channel grouping and revenue
train %>% 
  bind_cols(as_tibble(train$transactionRevenue)) %>% 
  group_by(channelGrouping) %>% 
  summarise(revenue = sum(value)) %>%
  ggplot(aes(x = channelGrouping, y = revenue)) +
  geom_point(color="steelblue", size=2) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#Users who came via Affiliates and Social channels do not generate revenue. The most profitable channel is Referral

#VisitNumber and revenue
train %>% 
  bind_cols(as_tibble(train$transactionRevenue)) %>% 
  group_by(visitNumber) %>% 
  summarise(revenue = sum(value)) %>%
  ggplot(aes(x = visitNumber, y = revenue)) +
  geom_point(color="steelblue", size=0.5) +
  theme_minimal() +
  scale_x_continuous(breaks=c(1, 3, 5, 10, 15, 25, 50, 100), limits=c(0, 105))+
  scale_y_continuous(labels = scales::comma)
#Generally first visit users generate more total revenue

#Browser and revenue
train %>% 
  bind_cols(as_tibble(train$transactionRevenue)) %>% 
  mutate(browser = factor(browser) %>% fct_lump(prop=0.01)) %>% 
  group_by(browser) %>% 
  summarize(revenue = sum(value)) %>% 
  ungroup() %>% 
  data.table::melt(id.vars = c("browser")) %>% 
  ggplot(aes(browser, value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ variable, scales = "free") + 
  theme_minimal() +
  labs(x = "browser", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") 
#Chrome users produce the highest total revenue

#Operating system and revenue
train %>% 
  bind_cols(as_tibble(train$transactionRevenue)) %>% 
  mutate(operatingSystem = factor(operatingSystem) %>% fct_lump(prop=0.01)) %>% 
  group_by(operatingSystem) %>% 
  summarize(revenue = sum(value)) %>% 
  ungroup() %>% 
  data.table::melt(id.vars = c("operatingSystem")) %>% 
  ggplot(aes(operatingSystem, value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ variable, scales = "free") + 
  theme_minimal() +
  labs(x = "operating system", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") 
#Mac, Windows and Chrome users generate the most of the revenue with Mac users being the highest

#Device category and revenue
train %>% 
  bind_cols(as_tibble(train$transactionRevenue)) %>% 
  mutate(deviceCategory = factor(deviceCategory) %>% fct_lump(prop=0.01)) %>% 
  group_by(deviceCategory) %>% 
  summarize(revenue = sum(value)) %>% 
  ungroup() %>% 
  data.table::melt(id.vars = c("deviceCategory")) %>% 
  ggplot(aes(deviceCategory, value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ variable, scales = "free") + 
  theme_minimal() +
  labs(x = "device category", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none")
#Desktop users generate the most revenue

#Replacing all NA values in character columns with Unknown and numeric columns with 0
train = train %>% mutate_all(funs(ifelse(is.character(.)&is.na(.), "Unknown", .)))
test = test %>% mutate_all(funs(ifelse(is.character(.)&is.na(.), "Unknown", .)))

train <- train %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0L, .)))
test <- test %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0L, .)))

#Converting categorical variables to factors 
factorVars = c("channelGrouping", "browser", "operatingSystem", "deviceCategory", "continent", "subContinent", "country", "region", "metro", "city", "networkDomain", "campaign", "source", "medium", "keyword", "referralPath", "adContent", "adwordsClickInfo.slot", "adwordsClickInfo.gclId", "adwordsClickInfo.adNetworkType")
train[, factorVars] <- lapply(train[, factorVars], as.factor)
test[, factorVars] <- lapply(test[, factorVars], as.factor)

#Grouping least common factor levels into "Other"
train$country = train$country %>% fct_lump(prop = 0.02)
train$subContinent = train$subContinent %>% fct_lump(prop = 0.05)
train$browser = train$browser %>% fct_lump(prop = 0.01)
train$operatingSystem = train$operatingSystem %>% fct_lump(prop = 0.01)
train$region = train$region %>% fct_lump(prop = 0.01)
train$metro = train$metro %>% fct_lump(prop = 0.01)
train$city = train$city %>% fct_lump(prop = 0.01)
train$networkDomain = train$networkDomain %>% fct_lump(prop = 0.01)
train$campaign = train$campaign %>% fct_lump(prop = 0.01)
train$source = train$source %>% fct_lump(prop = 0.01)
train$keyword = train$keyword %>% fct_lump(prop = 0.01)
train$referralPath = train$referralPath %>% fct_lump(prop = 0.01)
train$adContent = train$adContent %>% fct_lump(prop = 0.01)
train$adwordsClickInfo.gclId = train$adwordsClickInfo.gclId %>% fct_lump(prop = 0.01)

test$country = test$country %>% fct_lump(prop = 0.02)
test$subContinent = test$subContinent %>% fct_lump(prop = 0.05)
test$browser = test$browser %>% fct_lump(prop = 0.01)
test$operatingSystem = test$operatingSystem %>% fct_lump(prop = 0.01)
test$region = test$region %>% fct_lump(prop = 0.01)
test$metro = test$metro %>% fct_lump(prop = 0.01)
test$city = test$city %>% fct_lump(prop = 0.01)
test$networkDomain = test$networkDomain %>% fct_lump(prop = 0.01)
test$campaign = test$campaign %>% fct_lump(prop = 0.01)
test$source = test$source %>% fct_lump(prop = 0.01)
test$keyword = test$keyword %>% fct_lump(prop = 0.01)
test$referralPath = test$referralPath %>% fct_lump(prop = 0.01)
test$adContent = test$adContent %>% fct_lump(prop = 0.01)
test$adwordsClickInfo.gclId = test$adwordsClickInfo.gclId %>% fct_lump(prop = 0.01)

#Converting columns to their appropriate type
train$date <- as.Date(train$date, origin = '1970-01-01')
test$date <- as.Date(test$date, origin = '1970-01-01')

train$visitStartTime <- as.POSIXct(train$visitStartTime, tz="UTC", origin='1970-01-01')
test$visitStartTime <- as.POSIXct(test$visitStartTime, tz="UTC", origin='1970-01-01')

#Regression Models

#Choosing features
cols = c("fullVisitorId", "channelGrouping", "deviceCategory", "continent", "country", "hits", "pageviews", "medium", "transactionRevenue")
tr = train[,cols]
te = test[,cols]
tr_fvid = tr$fullVisitorId
te_fvid = te$fullVisitorId
tr$fullVisitorId = NULL
te$fullVisitorId = NULL
#Fitting the model
ols = lm(log1p(transactionRevenue) ~ ., data = tr)
#Correcting new levels in test set
te$country[te$country=='Taiwan'] = "Other"
te$browser[te$browser=='Opera Mini'] = "Other"
te$operatingSystem[te$operatingSystem=='Unknown'] = "Other"
#Making predictions
preds = predict(ols, newdata = te)
new_preds = ifelse(preds < 0, 0, expm1(preds))
sub <- data.frame(te_fvid, new_preds)
colnames(sub) <- c("fullVisitorId","PredictedLogRevenue")
sub = sub %>% group_by(fullVisitorId) %>% summarise(PredictedLogRevenue = log1p(sum(PredictedLogRevenue)))
write_csv(sub, "lmsubmission.csv")
#1.9189 score

#Adding more features
cols = c("fullVisitorId", "channelGrouping", "deviceCategory", "continent", "country", "hits", "pageviews", "medium", "transactionRevenue", "visitNumber", "timeOnSite", "browser", "operatingSystem")
tr = train[,cols]
te = test[,cols]
tr_fvid = tr$fullVisitorId
te_fvid = te$fullVisitorId
tr$fullVisitorId = NULL
te$fullVisitorId = NULL
#Fitting the model
ols = lm(log1p(transactionRevenue) ~ ., data = tr)
#Correcting new levels in test set
te$country[te$country=='Taiwan'] = "Other"
te$browser[te$browser=='Opera Mini'] = "Other"
te$operatingSystem[te$operatingSystem=='Unknown'] = "Other"
#Making predictions
preds = predict(ols, newdata = te)
new_preds = ifelse(preds < 0, 0, expm1(preds))
sub <- data.frame(te_fvid, new_preds)
colnames(sub) <- c("fullVisitorId","PredictedLogRevenue")
sub = sub %>% group_by(fullVisitorId) %>% summarise(PredictedLogRevenue = log1p(sum(PredictedLogRevenue)))
write_csv(sub, "lmsubmission2.csv")
#1.9174 score

#LASSO Regression

y.train = log1p(tr$transactionRevenue)
te$transactionRevenue = NULL
tr$transactionRevenue = NULL
#One Hot Encoding for categorical variables
X.train = model.matrix(~channelGrouping + deviceCategory + continent + country + medium + browser + operatingSystem, tr)
X.test = model.matrix(~channelGrouping + deviceCategory + continent + country + medium + browser + operatingSystem, te)
fit.lasso = cv.glmnet(X.train, y.train, family="gaussian", alpha=0, type.measure = "mse", nfolds = 5)
pred_glm <- predict(fit.lasso, X.test, s = "lambda.min") %>% c()
new_pred_glm = ifelse(pred_glm < 0, 0, expm1(pred_glm))
sub <- data.frame(te_fvid, new_pred_glm)
colnames(sub) <- c("fullVisitorId","PredictedLogRevenue")
sub = sub %>% group_by(fullVisitorId) %>% summarise(PredictedLogRevenue = log1p(sum(PredictedLogRevenue)))
write_csv(sub, "lassosubmission.csv")
#2.0937 score

#Regression after One Hot Encoding
x.train = as.data.frame(X.train)
x.test = as.data.frame(X.test)
ols = lm(y.train ~ ., data = x.train)
#Correcting column differences in train and test
x.train$countryCanada = NULL
x.train$countryVietnam = NULL
x.test$countryTaiwan = NULL
x.test$`browserOpera Mini` = NULL
x.test$operatingSystemUnknown = NULL
x.train$browserChrome = NULL
#Making predictions
preds = predict(ols, newdata = x.test)
new_preds = ifelse(preds < 0, 0, expm1(preds))
sub <- data.frame(te_fvid, new_preds)
colnames(sub) <- c("fullVisitorId","PredictedLogRevenue")
sub = sub %>% group_by(fullVisitorId) %>% summarise(PredictedLogRevenue = log1p(sum(PredictedLogRevenue)))
write_csv(sub, "lmsubmission3.csv")
#2.0168 score

#Spline Based Regression
spline = earth(y.train ~ ., data = x.train)
spline.pred =  predict(spline, newdata = x.test)
new_spline_preds = ifelse(spline.pred < 0, 0, expm1(spline.pred))
sub <- data.frame(te_fvid, new_spline_preds)
colnames(sub) <- c("fullVisitorId","PredictedLogRevenue")
sub = sub %>% group_by(fullVisitorId) %>% summarise(PredictedLogRevenue = log1p(sum(PredictedLogRevenue)))
write_csv(sub, "splinesubmission.csv")
#2.0288 score


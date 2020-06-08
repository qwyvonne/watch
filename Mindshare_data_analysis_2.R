#Brand Health Metrics score 
#Buzz
library(readxl)
buzz = read_xlsx("/Users/qingyuanwang/Documents/Mindshare Case Competition/Brand Health Metrics.xlsx",sheet = 1)
buzz = buzz[-c(1:6),]
buzz = na.omit(buzz)
colnames(buzz) = c("Week","buzz_score","Positive","Negative","Neutral","Unaware","Volume")
buzz
#Impression
impression = read_xlsx("/Users/qingyuanwang/Documents/Mindshare Case Competition/Brand Health Metrics.xlsx",sheet = 2)
impression = impression[-c(1:6),]
impression = na.omit(impression)
colnames(impression) = c("Week","impression_score","Positive","Negative","Neutral","Unaware","Volume")
impression
#quality
quality = read_xlsx("/Users/qingyuanwang/Documents/Mindshare Case Competition/Brand Health Metrics.xlsx",sheet = 3)
quality = quality [-c(1:6),]
quality  = na.omit(quality)
colnames(quality) = c("Week","quality_score","Positive","Negative","Neutral","Unaware","Volume")
quality
#value
value = read_xlsx("/Users/qingyuanwang/Documents/Mindshare Case Competition/Brand Health Metrics.xlsx",sheet = 4)
value = value [-c(1:6),]
value  = na.omit(value)
colnames(value) = c("Week","value_score","Positive","Negative","Neutral","Unaware","Volume")
value
#reputation
reputation = read_xlsx("/Users/qingyuanwang/Documents/Mindshare Case Competition/Brand Health Metrics.xlsx",sheet = 5)
reputation = reputation [-c(1:6),]
reputation = na.omit(reputation)
colnames(reputation) = c("Week","reputation_score","Positive","Negative","Neutral","Unaware","Volume")
reputation
#satisfaction
satisfaction = read_xlsx("/Users/qingyuanwang/Documents/Mindshare Case Competition/Brand Health Metrics.xlsx",sheet = 6)
satisfaction = satisfaction [-c(1:6),]
satisfaction = na.omit(satisfaction)
colnames(satisfaction) = c("Week","satisfaction_score","Positive","Negative","Neutral","Unaware","Volume")
satisfaction
#recommend
recommend = read_xlsx("/Users/qingyuanwang/Documents/Mindshare Case Competition/Brand Health Metrics.xlsx",sheet = 7)
recommend = recommend[-c(1:6),]
recommend = na.omit(recommend)
colnames(recommend) = c("Week","recommend_score","Positive","Negative","Neutral","Unaware","Volume")
recommend
#awareness 
awareness = read_xlsx("/Users/qingyuanwang/Documents/Mindshare Case Competition/Brand Health Metrics.xlsx",sheet = 8)
awareness = awareness[-c(1:6),]
awareness = na.omit(awareness)
colnames(awareness) = c("Week","awareness_score","Yes","No","Volume")
awareness
#attention
attention = read_xlsx("/Users/qingyuanwang/Documents/Mindshare Case Competition/Brand Health Metrics.xlsx",sheet = 9)
attention = attention[-c(1:6),]
attention = na.omit(attention)
colnames(attention) = c("Week","attention_score","Yes","No","Volume")
attention
#ad_awareness
ad_awareness = read_xlsx("/Users/qingyuanwang/Documents/Mindshare Case Competition/Brand Health Metrics.xlsx",sheet = 10)
ad_awareness= ad_awareness[-c(1:6),]
ad_awareness = na.omit(ad_awareness)
colnames(ad_awareness) = c("Week","ad_awareness_score","Yes","No","Volume")
ad_awareness
#WOM_exposure
WOM_exposure = read_xlsx("/Users/qingyuanwang/Documents/Mindshare Case Competition/Brand Health Metrics.xlsx",sheet = 11)
WOM_exposure= WOM_exposure[-c(1:6),]
WOM_exposure = na.omit(WOM_exposure)
colnames(WOM_exposure) = c("Week","WOM_exposure_score","Yes","No","Volume")
WOM_exposure
#consideration
consideration = read_xlsx("/Users/qingyuanwang/Documents/Mindshare Case Competition/Brand Health Metrics.xlsx",sheet = 12)
consideration = consideration[-c(1:6),]
consideration = na.omit(consideration)
colnames(consideration) = c("Week","consideration_score","Yes","No","Volume")
consideration
#purchase_intent
purchase_intent = read_xlsx("/Users/qingyuanwang/Documents/Mindshare Case Competition/Brand Health Metrics.xlsx",sheet = 13)
purchase_intent = purchase_intent[-c(1:6),]
purchase_intent = na.omit(purchase_intent)
colnames(purchase_intent) = c("Week","purchase_intent_score","Yes","No","Volume")
purchase_intent
#current_customer
current_customer = read_xlsx("/Users/qingyuanwang/Documents/Mindshare Case Competition/Brand Health Metrics.xlsx",sheet = 14)
current_customer =current_customer[-c(1:6),]
current_customer = na.omit(current_customer)
colnames(current_customer) = c("Week","current_customer_score","Yes","No","Volume")
current_customer
#former_customer
former_customer = read_xlsx("/Users/qingyuanwang/Documents/Mindshare Case Competition/Brand Health Metrics.xlsx",sheet = 15)
former_customer = former_customer[-c(1:6),]
former_customer = na.omit(former_customer)
colnames(former_customer) = c("Week","former_customer_score","Yes","No","Volume")
former_customer
library(dplyr)
#JOIN the score of each sheet
a = left_join(buzz,impression,by = "Week")
b = left_join(a,quality,by = "Week")
c = left_join(b,value,by = "Week")
d = left_join(c,reputation,by = "Week")
e = left_join(d,satisfaction,by = "Week")
f = left_join(e,recommend,by = "Week")
g = left_join(f,awareness,by = "Week")
h = left_join(g,attention, by = "Week")
i = left_join(h,ad_awareness, by = "Week")
j = left_join(i,WOM_exposure, by = "Week")
k = left_join(j,consideration, by = "Week")
l = left_join(k,purchase_intent, by = "Week")
m = left_join(l,current_customer, by = "Week")
n = left_join(m,former_customer, by = "Week")
summary(n)
n$Week = as.numeric(n$Week)
n$Week = as.Date(n$Week, origin = "1899-12-30")
#linear regression 

#model1 = lm(purchase_intent_score~satisfaction_score,data = train)
#summary(model1)
#Returned NA in error and t-value and Pr(>|t|) 
# First check NA in data → sum(is.na(n)) → No NA
# Second check correlation between variables 
# Mistakes found: variable data type is not numeric
#Converting variables to numeric variables 
n$buzz_score = as.numeric(n$buzz_score)
n$impression_score = as.numeric(n$impression_score)
n$quality_score = as.numeric(n$quality_score)
n$value_score = as.numeric(n$value_score)
n$reputation_score = as.numeric(n$reputation_score)
n$satisfaction_score = as.numeric(n$satisfaction_score)
n$recommend_score = as.numeric(n$recommend_score)
n$awareness_score = as.numeric(n$awareness_score)
n$attention_score = as.numeric(n$attention_score)
n$ad_awareness_score = as.numeric(n$ad_awareness_score)
n$WOM_exposure_score = as.numeric(n$WOM_exposure_score)
n$consideration_score = as.numeric(n$consideration_score)
n$purchase_intent_score = as.numeric(n$purchase_intent_score)
n$current_customer_score = as.numeric(n$current_customer_score)
n$former_customer_score = as.numeric(n$former_customer_score)
n$purchase_intent_score = as.numeric(n$purchase_intent_score)

na.omit(n)
set.seed(1706)
split = sample(x = nrow(n),size = 0.8*nrow(n))
train = n[split,]
test = n[-split,]

#Model 1 - linear regression: buzz, impression 
cor(buzz_score,impression_score)
model1 = lm(purchase_intent_score ~ buzz_score + impression_score, data = train)
summary(model1)
pred1 = predict(model1)
#Calculate RMSE of model 1: 
sse1 = sum((pred1-train$purchase_intent_score)^2)
sst1 = sum((mean(train$purchase_intent_score) - train$purchase_intent_score)^2)
model1_r2 = 1-sse1/sst1; model1_r2 # r^2 = 0.054
rmse1 = sqrt(mean((pred1-train$purchase_intent_score)^2)); rmse1 #1.657222
#prediction on test 
pred1_test = predict(model1, newdata=test)
sse1_test = sum((pred1_test - test$purchase_intent_score)^2)
sst1_test = sum((mean(train$purchase_intent_score)-test$purchase_intent_score)^2)
model1_r2_test = 1 - sse1_test/sst1_test; model1_r2_test  #0.06609556
rmse1_test = sqrt(mean((pred1_test-test$purchase_intent_score)^2)); rmse1_test #1.706298

#Model 2 - linear regression: buzz, impression, quality
cor(buzz_score, quality_score); cor(impression_score,quality_score);
model2 = lm(purchase_intent_score ~ buzz_score + impression_score + quality_score, data = train)
summary(model2)
pred2 = predict(model2)
sse2 = sum((pred2 - train$purchase_intent_score)^2)
sst2 = sum((mean(train$purchase_intent_score) - train$purchase_intent_score)^2)
model2_r2 = 1-sse2/sst2; model2_r2 #r^2 = 0.064
rmse2 = sqrt(mean((pred2-train$purchase_intent_score)^2)); rmse2 #1.647696

#RMSE on test
pred2_test = predict(model2, newdata=test)
sse2_test = sum((pred2_test - test$purchase_intent_score)^2)
sst2_test = sum((mean(train$purchase_intent_score)-test$purchase_intent_score)^2)
model2_r2_test = 1 - sse2_test/sst2_test; model2_r2_test  # 0.09171006
rmse2_test = sqrt(mean((pred2_test-test$purchase_intent_score)^2)); rmse2_test #1.682736

#Model 3 - linear regression ~ all features 
model3 = lm(purchase_intent_score ~ buzz_score + impression_score + quality_score + value_score + 
              reputation_score + satisfaction_score + recommend_score + awareness_score + 
              attention_score + ad_awareness_score + WOM_exposure_score + consideration_score, data = train)
summary(model3)
pred3 = predict(model3)
sse3 = sum((pred3 - train$purchase_intent_score)^2)
sst3 = sum((mean(train$purchase_intent_score) - train$purchase_intent_score)^2)
model3_r2 = 1-sse3/sst3; model3_r2 # r^2 = 0.173333
pred3_test = predict(model3, newdata=test)
sse3_test = sum((pred3_test - test$purchase_intent_score)^2)
sst3_test = sum((mean(train$purchase_intent_score)-test$purchase_intent_score)^2)
model3_r2_test = 1 - sse3_test/sst3_test; model3_r2_test  #  0.2851017
rmse3_test = sqrt(mean((pred3_test-test$purchase_intent_score)^2)); rmse3_test #1.492884

#Random Forest 
library(randomForest)
library(rpart)
library(rpart.plot)
library(caret)
set.seed(100)
forest = randomForest(purchase_intent_score ~ buzz_score + impression_score + quality_score + 
                        value_score + reputation_score + satisfaction_score + recommend_score + 
                        awareness_score + attention_score + ad_awareness_score + 
                        WOM_exposure_score + consideration_score, data = train, ntree = 1000)
print(forest)
plot(forest)

#See variable importance
varImpPlot(forest); importance(forest)
getTree(forest,k = 100,labelVar = TRUE)
hist(treesize(forest))
predForest = predict(forest, newdata = test)
rmseForest = sqrt(mean((predForest-test$purchase_intent_score)^2)); rmseForest #1.32313

#Random Forest with Cross Validation 
trControl = trainControl(method = "cv", number = 10)
tuneGrid = expand.grid(mtry = 1:5)
set.seed(100)
cvForest = train(purchase_intent_score ~ buzz_score + impression_score + quality_score + 
                   value_score + reputation_score + satisfaction_score + recommend_score + 
                   awareness_score + attention_score + ad_awareness_score + 
                   WOM_exposure_score + consideration_score, data = train, method = "rf", ntree = 1000,
                 trControl = trControl, tuneGrid = tuneGrid)
cvForest #best mtry is 3
set.seed(100)
forest = randomForest(purchase_intent_score ~ buzz_score + impression_score + quality_score + 
                        value_score + reputation_score + satisfaction_score + recommend_score + 
                        awareness_score + attention_score + ad_awareness_score + 
                        WOM_exposure_score + consideration_score, data = train, ntree = 1000,
                      mtry = 2)
predForest = predict(forest,newdata = test)
rmseForest = sqrt(mean((predForest - test$purchase_intent_score)^2)); rmseForest #1.334768

#Boosting
library(gbm)
set.seed(100)
boost = gbm(purchase_intent_score ~ buzz_score + impression_score + quality_score + 
              value_score + reputation_score + satisfaction_score + recommend_score + 
              awareness_score + attention_score + ad_awareness_score + 
              WOM_exposure_score + consideration_score, data = train, distribution = "gaussian",
            n.trees = 100000, interaction.depth = 3, shrinkage = 0.001)
predBoostTrain = predict(boost, n.trees = 100000)
rmseBoostTrain = sqrt(mean((predBoostTrain-train$purchase_intent_score)^2)); rmseBoostTrain
predBoost = predict(boost,newdata = test, n.trees = 10000)
rmseBoost = sqrt(mean((predBoost - test$purchase_intent_score)^2)); rmseBoost #1.3062
summary(boost)

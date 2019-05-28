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

#Customer Segmentation 
#Clustering 
library(dplyr)
seg = read.csv("/Users/qingyuanwang/Documents/Mindshare Case Competition/cleaned.csv")
str(seg)
#Cleaning Data

#Online_shopping
seg$I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Disagree.completely
seg$I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Disagree.somewhat = 2*seg$I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Disagree.somewhat
seg$I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Agree.somewhat = 3*seg$I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Agree.somewhat
seg$I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Agree.completely = 4*seg$I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Agree.completely
seg$online_shopping = seg %>%
  select(`I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Disagree.completely`,
         `I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Disagree.somewhat`,
         `I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Agree.somewhat`,
         `I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Agree.completely`) %>%
  rowSums()
seg$online_shopping

#loyalty
seg$I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Disagree.completely
seg$I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Disagree.somewhat = 2*seg$I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Disagree.somewhat
seg$I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Agree.somewhat = 3*seg$I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Agree.somewhat
seg$I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Agree.completely = 4*seg$I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Agree.completely
seg$loyalty = seg %>% 
  select(`I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Disagree.completely`,
         `I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Disagree.somewhat`,
         `I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Agree.somewhat`,
         `I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Agree.completely`) %>%
  rowSums()
seg$loyalty

#price_sensitive
seg$I.only.spend.what.I.budget.on.fashion.items...Disagree.completely
seg$I.only.spend.what.I.budget.on.fashion.items...Disagree.somewhat = 2*seg$I.only.spend.what.I.budget.on.fashion.items...Disagree.somewhat
seg$I.only.spend.what.I.budget.on.fashion.items...Agree.somewhat = 3*seg$I.only.spend.what.I.budget.on.fashion.items...Agree.somewhat
seg$I.only.spend.what.I.budget.on.fashion.items...Agree.completely = 4*seg$I.only.spend.what.I.budget.on.fashion.items...Agree.completely
seg$price_sensitive = seg %>%
  select(`I.only.spend.what.I.budget.on.fashion.items...Disagree.completely`,
         `I.only.spend.what.I.budget.on.fashion.items...Disagree.somewhat`,
         `I.only.spend.what.I.budget.on.fashion.items...Agree.somewhat`,
         `I.only.spend.what.I.budget.on.fashion.items...Agree.completely`) %>%
  rowSums()
seg$price_sensitive

#seasonality
seg$I.buy.new.clothes.at.the.beginning.of.each.season...Disagree.completely
seg$I.buy.new.clothes.at.the.beginning.of.each.season...Disagree.somewhat = 2*seg$I.buy.new.clothes.at.the.beginning.of.each.season...Disagree.somewhat
seg$I.buy.new.clothes.at.the.beginning.of.each.season...Agree.somewhat = 3*seg$I.buy.new.clothes.at.the.beginning.of.each.season...Agree.somewhat
seg$I.buy.new.clothes.at.the.beginning.of.each.season...Agree.completely = 4*seg$I.buy.new.clothes.at.the.beginning.of.each.season...Agree.completely
seg$seasonality = seg %>%
  select(`I.buy.new.clothes.at.the.beginning.of.each.season...Disagree.completely`,
         `I.buy.new.clothes.at.the.beginning.of.each.season...Disagree.somewhat`,
         `I.buy.new.clothes.at.the.beginning.of.each.season...Agree.somewhat`,
         `I.buy.new.clothes.at.the.beginning.of.each.season...Agree.completely`) %>%
  rowSums()
seg$seasonality

#look(is important than brand)
seg$When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Disagree.completely
seg$When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Disagree.somewhat = 2*seg$When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Disagree.somewhat
seg$When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Agree.somewhat = 3*seg$When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Agree.somewhat
seg$When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Agree.completely = 4*seg$When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Agree.completely
seg$look = seg %>%
  select(`When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Disagree.completely`,
         `When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Disagree.somewhat`,
         `When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Agree.somewhat`,
         `When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Agree.completely`) %>%
  rowSums()
seg$look

#Hierarchical clustering
# data_cluster = seg[,c("online_shopping","loyalty","price_sensitive","seasonality","look")]
#original data set exceed memory, so to draw 1/10 original dataset sample = 4888
#First column wgtpop is Number of respondents. Wgtpop=3 means 3 users responded in this manner
#Each row has different weight, which may impact clustering result, need to deal with it

sample_data_cluster = seg[sample(nrow(seg), 4888),]

cared_cols = c("online_shopping","loyalty","price_sensitive","seasonality","look")

#scaling 
sample_data_cluster_scaled = scale(sample_data_cluster[, cared_cols])
d = dist(sample_data_cluster_scaled, method = "euclidean")
clusters = hclust(d = d, method = "ward.D2")
plot(clusters)
cor(cophenetic(clusters),d) # 0.4111652 indicates moderate fit
#only display the tree above 30 to clarify picture, 2 or 3 cluster solution looks good, also take a look at 4
plot(cut(as.dendrogram(clusters),h=30)$upper)
#two clusters solution
plot(clusters)
rect.hclust(tree = clusters, k = 2, border = "tomato")
#three cluster solution 
plot(clusters)
rect.hclust(tree = clusters, k = 3, border = "tomato")
#four cluster solution 
plot(clusters)
rect.hclust(tree = clusters, k = 4, border = "tomato")

#add h_segments to sample_data_cluster, the one that was not scaled
h_segments = cutree(tree = clusters, k = 3)
table(h_segments) 
sample_data_cluster$h_segments = factor(h_segments) 

#profile cluster by age
sample_data_cluster_1 = sample_data_cluster %>% 
  #select(wgtpop,`Age.18.24`,`Age.25.34`,`Age.35.44`,`Age.45.54`,`Age.55.64`,`Age.65.`) %>% 
  mutate(`Age.18.24_people`= wgtpop*`Age.18.24`,`Age.25.34_people`= wgtpop*`Age.25.34`,
         `Age.35.44_people`= wgtpop*`Age.35.44`, `Age.45.54_people`= wgtpop*`Age.45.54`,
         `Age.55.64_people`= wgtpop*`Age.55.64`,`Age.65._people`= wgtpop*`Age.65.`)
  
result_df = sample_data_cluster_1 %>% 
  group_by(h_segments) %>%
  summarise(`18 to 24` = sum(`Age.18.24_people`), `25 to 34` = sum(`Age.25.34_people`),
            `35 to 44` = sum(`Age.35.44_people`),`45 to 54` = sum(`Age.45.54_people`),
            `55 to 64` = sum(`Age.55.64_people`),`65 +` = sum(`Age.65._people`)) %>%
  tbl_df()

#Profile cluster by level of education 
result_df2 = sample_data_cluster %>% 
  mutate(`Educ..post.graduate_people` = wgtpop*`Educ..post.graduate`,
         `Educ..graduated.college.plus_people` = wgtpop*`Educ..graduated.college.plus`,
         `Educ..attended.college` = wgtpop*`Educ..attended.college`,
         `Educ..no.college` = wgtpop*`Educ..no.college`,
         `Educ..graduated.high.school` = wgtpop*`Educ..graduated.high.school`,
         `Educ..did.not.graduate.HS` = wgtpop*`Educ..did.not.graduate.HS`) %>%
  group_by(h_segments) %>%
  summarise(`post graduate` = sum(`Educ..post.graduate_people`),  
            `graduated college` = sum(`Educ..graduated.college.plus_people`),
            `attended college` = sum(`Educ..attended.college`),
            `no college` = sum(`Educ..no.college`),
            `graduated high school` = sum(`Educ..graduated.high.school`),
            `did not graduated high school` = sum(`Educ..did.not.graduate.HS`)) %>%
  tbl_df()
#We know we will end with three clusters(3 products), choose k = 3
library(psych)
library(ggplot2)
temp = data.frame(cluster = factor(h_segments),
                  factor1 = fa(sample_data_cluster,nfactors = 5,rotate = "varimax")$scores[,1],
                  factor2 = fa(sample_data_cluster,nfactors = 5,rotate = 'varimax')$scores[,2])
ggplot(temp, aes(x=factor1, y = factor2, col = cluster))+
  geom_point()

#Similarly 
library(cluster)
clusplot(sample_data_cluster,
         h_segments,
         color = T,shade= T,labels = 4, lines = 0, main = "Hierarchical Cluster Plot")

# K-means clustering 
set.seed(617)
km = kmeans(x = data_cluster,centers = 3,iter.max = 10000, nstart = 25)
table(km$cluster)

within_ss = sapply(1:10,FUN = function(x) kmeans(x = data_cluster,centers = x,iter.max = 1000,nstart = 25)$tot.withinss)
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))
#Silhouette Plot indicates a cluster of 4 to be the optimal, since we need 3 clusters for 3 product, we just take a look at 4 cluster
set.seed(617)

km = kmeans(x = data_cluster,centers = 4,iter.max=10000,nstart=25)
k_segments = km$cluster
table(k_segments)

km = kmeans(x = data_cluster,centers = 3,iter.max=10000,nstart=25)
k_segments = km$cluster
table(k_segments)

library(psych)
temp = data.frame(cluster = factor(k_segments),
                  factor1 = fa(data_cluster,nfactors = 5,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 5,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

library(cluster)
clusplot(data_cluster,
         k_segments,
         color=T,shade=T,labels=4,lines=0,main='k-means Cluster Plot')

#Model based clustering
library(mclust)
clusters_mclust = Mclust(data_cluster)
summary(clusters_mclust)
clusters_mclust_3 = Mclust(data_cluster,G=3)
summary(clusters_mclust_3)
clusters_mclust_3$bic
mclust_bic = -sapply(1:10,FUN = function(x) Mclust(data_cluster,G=x)$bic)
mclust_bic
ggplot(data=data.frame(cluster = 1:10,bic = mclust_bic),aes(x=cluster,y=bic))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))
#the plot indicates 9 clusters, but not feasible, take a look at 3,4 cluster solution
#3 cluster solution
m_clusters = Mclust(data = data_cluster,G = 3)
m_segments = m_clusters$classification
table(m_segments)
library(psych)
temp = data.frame(cluster = factor(m_segments),
                  factor1 = fa(data_cluster,nfactors = 5,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 5,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

#4 cluster solution
m_clusters_4 = Mclust(data = data_cluster,G = 4)
m_segments_4 = m_clusters_4$classification
table(m_segments_4)
library(psych)
temp = data.frame(cluster = factor(m_segments_4),
                  factor1 = fa(data_cluster,nfactors = 5,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 5,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

library(cluster)
clusplot(data_cluster,
         m_segments,
         color=T,shade=T,labels=4,lines=0,main='mclust Cluster Plot')

table(h_segments)
table(k_segments)
table(m_segments)

# Other way to profile hierarchical clusters 
seg$Men = factor(seg$Men, labels = c("woman", "man"))
seg$Women = factor(seg$Women, labels = c("man","woman"))

seg$Educ..post.graduate = factor(seg$Educ..post.graduate, labels = c("others","post graduate"))seg$Educ..attended.college = factor(seg$Educ..attended.college, labels = c("other","attend college"))
seg$Educ..graduated.college.plus = factor(seg$Educ..graduated.college.plus, labels = c("other","graduate college plus"))
seg$Educ..attended.college = factor(seg$Educ..attended.college, labels = c("other","attended college")) 
seg$Educ..no.college = factor(seg$Educ..no.college, labels = c("other","no college"))
seg$Educ..graduated.high.school = factor(seg$Educ..graduated.high.school, labels = c("other","high school"))
seg$Educ..did.not.graduate.HS = factor(seg$Educ..did.not.graduate.HS, labels = c("other","did not graduate high school"))

seg$Age.18.24 = factor(seg$Age.18.24, labels = c("other","18 - 24"))
seg$Age.25.34 = factor(seg$Age.25.34, labels = c("other", "25 - 34"))
seg$Age.35.44 = factor(seg$Age.35.44, labels = c("other","35 - 44"))
seg$Age.45.54 = factor(seg$Age.45.54, labels = c("other","45 - 54"))
seg$Age.55.64 = factor(seg$Age.55.64, labels = c("other","55 - 64"))
seg$Age.65. = factor(seg$Age.65., labels = c("other","65 and above"))

sample_data = seg[sample(nrow(seg), 4888),]
data_cluster2 = cbind.data.frame(sample_data, h_segments)

#Profile segment by gender
prop.table(table(data_cluster2$h_segments,data_cluster2[,"Men"]),1)

#Profile by education 
prop.table(table(data_cluster2$h_segments,data_cluster2[,"Educ..post.graduate"]),1)
prop.table(table(data_cluster2$h_segments,data_cluster2[,"Educ..graduated.college.plus"]),1)
prop.table(table(data_cluster2$h_segments,data_cluster2[,"Educ..attended.college"]),1)
prop.table(table(data_cluster2$h_segments,data_cluster2[,"Educ..no.college"]),1)
prop.table(table(data_cluster2$h_segments,data_cluster2[,"Educ..graduated.high.school"]),1)
prop.table(table(data_cluster2$h_segments,data_cluster2[,"Educ..did.not.graduate.HS"]),1)

#Profile by age
prop.table(table(data_cluster2$h_segments,data_cluster2[,"Age.18.24"]),1)
prop.table(table(data_cluster2$h_segments,data_cluster2[,"Age.25.34"]),1)
prop.table(table(data_cluster2$h_segments,data_cluster2[,"Age.35.44"]),1)
prop.table(table(data_cluster2$h_segments,data_cluster2[,"Age.45.54"]),1)
prop.table(table(data_cluster2$h_segments,data_cluster2[,"Age.55.64"]),1)
prop.table(table(data_cluster2$h_segments,data_cluster2[,"Age.65."]),1)

#Competitive Spending 
spending = read.csv("/Users/qingyuanwang/Documents/Mindshare Case Competition/case_competition_competitive_spend.csv")
#converting wide data to tall data using gather()
library(tidyr)
spending_tall = 
  spending %>% 
  gather("Date","Competitive Spending",2:105)
spending_tall
summary(spending_tall$Date)
class(spending_tall$Date)
library(stringr)
spending_tall$Date = str_remove(spending_tall$Date,"X") 
#https://www.statmethods.net/input/dates.html %Y four digits year
spending_tall$Date = as.Date(spending_tall$Date, '%m.%d.%Y')
#Filtering out CityDweller and plot the spending 
library(dplyr)
citydweller_spending = 
spending_tall %>% 
  filter(Competitors.for == "CityDweller") %>%
  group_by(Competitors.for) 
citydweller_spending
library(ggplot2)
ggplot(citydweller_spending, aes(x = Date, y = `Competitive Spending`)) +
  geom_point() +
  ggtitle("CityDweller Competitive Spending")

#Filtering out WaveMaker and plot the spending 
wavemaker_spending = 
  spending_tall %>% 
  filter(Competitors.for == "WaveMaker") %>%
  group_by(Competitors.for) 
wavemaker_spending
ggplot(wavemaker_spending, aes(x = Date, y = `Competitive Spending`)) + 
  geom_point() + 
  ggtitle("WaveMaker Competitive Spending")

#Filtering out TimePeace and plot the spending 
timepeace_spending = 
  spending_tall %>% 
  filter(Competitors.for == "TimePeace") %>%
  group_by(Competitors.for) 
timepeace_spending
library(ggplot2)
ggplot(timepeace_spending, aes(x = Date, y = `Competitive Spending`)) +
  geom_point() + 
  ggtitle("TimePeace Competitive Spending")

library(readxl)
setwd("/Users/qingyuanwang/Documents/Mindshare Case Competition/")
search_interest = read_xlsx("Product Search Interest Index.xlsx",col_names = TRUE,skip = 2)
search_interest[search_interest == ""] <- NA
search_interest = na.omit(search_interest)

#Search Interest Distribution 
ggplot(search_interest, aes(x=search_interest$CityDweller)) + 
  geom_histogram(color="black", fill="pink",stat="count") +
  xlab("CityDweller Search Interest") +
  ylab("Number of Search")

ggplot(search_interest, aes(x=search_interest$WaveMaker)) + 
  geom_histogram(color="black", fill="blue",stat="count") +
  xlab(" WaveMaker Search Interest") +
  ylab("Number of Search")

ggplot(search_interest, aes(x=search_interest$TimePeace)) + 
  geom_histogram(color="black", fill="yellow",stat="count") +
  xlab("TimePeace Search Interest") +
  ylab("Number of Search")

# Interest of each product through the year 
ggplot(search_interest,aes(x = Week, y = CityDweller)) + 
  geom_point(position = "jitter") +
  ggtitle("CityDweller Search Interest")

ggplot(search_interest,aes(x = Week, y = WaveMaker)) + 
  geom_point(position = "jitter") +
  ggtitle("WaveMaker Search Interest")

ggplot(search_interest,aes(x = Week, y = TimePeace)) + 
  geom_point(position = "jitter") +
  ggtitle("TimePeace Search Interest")

#basic plotting method
plot(search_interest$CityDweller) # seasonal
plot(search_interest$WaveMaker) # increasing trend
plot(search_interest$TimePeace) #flat(0), then a peak 

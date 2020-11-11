library(readr)
library(data.table)
library(ggplot2)
library(reshape2)
library(extrafont)
library(stringr)
library(tidyr)
library(e1071)
library(nnet)
library(dplyr)
library(caret)
library(tidyr)


###### CHOOSE TIME PERIOD (based on forward citation and patent renewal)


### BASED ON PATENT QUALITY INDEX 6 (FORWARD CITATIONS)
# OECD EPO_Inv_reg data
system.time(EPO_Inv_reg <- read_delim("REGPAT/202001_EPO_Inv_reg.txt","|", 
                                      escape_double = FALSE, trim_ws = TRUE))
# Take only applications from Europe countries (https://europa.eu/european-union/about-eu/countries_en#tab-0-1)
EU_ctry_code <- c("AT", "BE", "BG", "HR", "CZ", "DK", "EE", "FI", "FR", "DE", 
                  "GR", "HU", "IS", "IT", "LV", "LT", "LU", "MT", "NL", "PL", 
                  "PT", "RO", "SK", "SI", "IS", "SE", "GB")
appln_id_EU <- unique(EPO_Inv_reg$appln_id[EPO_Inv_reg$ctry_code %in% EU_ctry_code])
# OECD EPO_IPC data
system.time(EPO_IPC <- read_delim("REGPAT/202001_EPO_IPC.txt","|", 
                                  escape_double = FALSE, trim_ws = TRUE))
# Application IDs by at least 1 inventor from EU countries along with their 
# country code for later use
id_prio_EU <- unique(EPO_IPC[EPO_IPC$appln_id %in% appln_id_EU, c("appln_id", "prio_year")])
min(id_prio_EU$prio_year) # 1942
max(id_prio_EU$prio_year) # 2019
# Count the number of patents in each year in the whole time period
Overall_look <- aggregate(id_prio_EU$appln_id, by = list(id_prio_EU$prio_year), FUN=length)
names(Overall_look) <- c('prio_year', 'patents_nbr')

## A graph showing the number of patents over time in EU from 1942 to 2019
# If you have not installed this package extrafont, please install it and run the code "font_import()"
plot_overall_look_1 <- ggplot(Overall_look, aes(x=prio_year, y=patents_nbr)) + 
  geom_point(fill="darkblue", color="darkred") +
  labs(x = "Priority Year", y = "Number of patents") + 
  ggtitle("Graph 1. Number of patents over time in EU from 1942 to 2019")
plot_overall_look_1 + theme_dark() + theme(plot.title = element_text(hjust = 0.5, size = 11, 
                                                                     face = "bold")) + 
  theme(text=element_text(family="Times New Roman", size=11)) +
  theme(axis.text = element_text(size = 11)) + theme(axis.title = element_text(size = 11))


### BASED ON PATENT RENEWAL
# OECD_PATENT_QUALITY_EPO_INDIC data
OECD_PATENT_QUALITY_EPO <- read_delim("Quality/202001_OECD_PATENT_QUALITY_EPO/202001_OECD_PATENT_QUALITY_EPO_INDIC.txt","|", 
                                      escape_double = FALSE, trim_ws = TRUE)
renewal_EU <- OECD_PATENT_QUALITY_EPO[OECD_PATENT_QUALITY_EPO$appln_id %in% appln_id_EU, 
                                      c("appln_id", "renewal")]
renewal_EU <- merge(x=renewal_EU, 
                    y=id_prio_EU, 
                    by="appln_id", all.y=TRUE)
# Take a look at the average renewals of patents in Germany over time
renewal_EU_avg <- aggregate(renewal_EU$renewal, by=list(renewal_EU$prio_year), FUN=mean, na.rm=T, 
                                na.action=T)
names(renewal_EU_avg) <- c("prio_year", "renewal_avg_nbr")
renewal_EU_avg <- renewal_EU_avg[!is.na(renewal_EU_avg$renewal_avg_nbr),]
min(renewal_EU_avg$prio_year) # which is 1942
max(renewal_EU_avg$prio_year) # which is 2018

## A graph showing the average renewals of patents in over time from 1977-2010
plot_overall_look_2 <- ggplot(renewal_EU_avg, aes(x=prio_year, y=renewal_avg_nbr)) + 
  geom_point(fill="darkblue", color="darkred") +
  labs(x = "Priority Year", y = "Average number of renewals") + 
  ggtitle("Graph 2. Average number of patent renewals over time in EU from 1977 to 2010")
plot_overall_look_2 + theme_dark() + theme(plot.title = element_text(hjust = 0.5, size = 12, 
                                                                     face = "bold")) + 
  theme(text=element_text(family="Times New Roman", size=12)) +
  theme(axis.text = element_text(size = 12)) + theme(axis.title = element_text(size = 12))

# choose patents from 1981-1995 for analysis





###### CHOOSE COUNTRIES which became members of EU in 1981 or before -> more homogenerous 
ctry_code_use <- c("BE", "DK", "FR", "DE", "GB", "GR", "IS", "IT", "LU", "NL")







#----------------------------------------------------------------------------






###### CREATE DATASET

### Application IDs by at least 1 inventor from 1 of 9 chosen EU countries 
# along with their country code for later use
appln_id_ctry <- unique(EPO_Inv_reg[EPO_Inv_reg$ctry_code %in% ctry_code_use, 
                                   c("appln_id", "ctry_code")])

### Application IDs by at least 1 inventor from 1 of 9 chosen
# EU countries from 1981 to 1995
appln_id_prio <- unique(EPO_IPC[EPO_IPC$appln_id %in% unique(appln_id_ctry$appln_id), c("appln_id", "prio_year")])
appln_id_prio_81_95 <- appln_id_prio[appln_id_prio$prio_year >= 1981 & appln_id_prio$prio_year<= 1995,]
appln_id_81_95 <- unique(appln_id_prio_81_95$appln_id)
write_csv(appln_id_prio_81_95,"appln_id_prio_81_95.csv") # for later use



#------------------------------------------------------------------------------








### Combine the numbers of citation and other quality indicators into a dataframe
quality_indicators <- OECD_PATENT_QUALITY_EPO[OECD_PATENT_QUALITY_EPO$appln_id %in% appln_id_81_95,c("appln_id", "renewal", "quality_index_4")]
# Delete the cases with missing data
quality_indicators <- quality_indicators[complete.cases(quality_indicators),]





### K-means cluster
# Prepare features
plot(density(quality_indicators$renewal))
plot(density(quality_indicators$quality_index_4))
# Because the features are not normal distributed, I don't use AVOVA-Test to choose 
#the number of clusters. I use Elbow Method instead. 
# feature scaling
scaling_func <- function(x){(x-min(x))/(max(x)-min(x))}
quality_indicators$renewal_scaled <- scaling_func(quality_indicators$renewal)
write_csv(quality_indicators, "quality_indicators")
# Try 2, 3, 4, 5, 6, 7, 8, 9 and 10 clusters
set.seed(123)
seed <- sample(1:200, 6, replace=FALSE)
par(mfrow = c(2, 3)) 
for (i in seed){
  set.seed(i)
  wss=vector()
  for (j in 2:10){
    quality_k_means_rs <- kmeans(quality_indicators[, c(3,4)], j)
    wss<- c(wss,quality_k_means_rs$tot.withinss)
  }
  plot(2:10, wss,
       type="b", pch = 11, frame = TRUE, 
       main = paste("with set.seed(", i,")", sep=""),
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
}
mtext("K-means cluster for patent quality", outer=TRUE,  cex=1, line=-1.5)
# 5 is a good choice




#------------------------------------------------ --------------------------------------

######## 5 clusters


##### predict quality of patents
# find the seed with the smallest total within-clusters sum of squares
no_clusters <- 5
wss=vector()
for (i in seed){
  set.seed(i)
  quality_k_means_rs <- kmeans(quality_indicators[, c(3,4)], no_clusters)
  wss<- c(wss,quality_k_means_rs$tot.withinss)
}
set.seed(seed[match(min(wss), wss)])
quality_predict <- cbind(quality_indicators, 
                         kmeans(quality_indicators[, c(3,4)], no_clusters)$cluster)
par(mfrow=c(1,1))
plot(quality_indicators[, c(3,4)],col = kmeans(quality_indicators[, c(3,4)], no_clusters)$cluster,
     xlab='Patent quality index 4',
     ylab='Number of renewals (scaled)')
mtext("K-means cluster for patent quality", outer=TRUE,  cex=1, line=-1.5)
names(quality_predict)[ncol(quality_predict)] <- "quality_group" 
# check the percentage of each quality group
nrow(quality_predict[quality_predict$quality_group==1,])/nrow(quality_predict)
nrow(quality_predict[quality_predict$quality_group==2,])/nrow(quality_predict)
nrow(quality_predict[quality_predict$quality_group==3,])/nrow(quality_predict)
nrow(quality_predict[quality_predict$quality_group==4,])/nrow(quality_predict)
nrow(quality_predict[quality_predict$quality_group==5,])/nrow(quality_predict)
# although the percentage of quality groups are much different, there is no
#quality group dominating the other groups in the number of observation. Therefore,
#there is almost no risk of 'Anormaly detection' problem
quality_predict_agg <- aggregate(quality_predict[, c(3,4)], 
                                by=list(quality_predict$quality_group), 
                                FUN=mean)
# change the number of quality level according to ascending values of 
#renewal_scaled
quality_predict_agg$rank <- NA
quality_predict_agg$rank[order(quality_predict_agg$renewal_scaled)] <- 1:nrow(quality_predict_agg)
old <- 1:nrow(quality_predict_agg)
new <- quality_predict_agg$rank
quality_predict$quality_group[quality_predict$quality_group %in% old] <- new[match(quality_predict$quality_group, old, nomatch = 0)]
# check it again
quality_predict_agg <- aggregate(quality_predict[, c(3,4)], 
                               by=list(quality_predict$quality_group), 
                               FUN=mean)
quality_output <- quality_predict
write_csv(quality_output, "quality_output.csv")
quality_predict <-quality_predict[, c(1, 5)] 







#-------------------------------------------------------------------------------------------------







### Merge features for the predicting model into 1 dataframe
# country code
no_col <- ncol(quality_predict)
quality_predict <- unique(merge(x=quality_predict, y=appln_id_ctry, 
                                by='appln_id', all.x = TRUE))
no_add_col <- length(levels(factor(quality_predict$ctry_code)))
quality_predict$ctry_code <- as.character(quality_predict$ctry_code)
quality_predict <- quality_predict %>% pivot_wider(names_from = ctry_code, values_from = ctry_code, 
                   values_fill = list(ctry_code = "0"))
quality_predict[, (no_col+1):(no_col+no_add_col)] <- data.frame(lapply(quality_predict[, (no_col+1):(no_col+no_add_col)], 
                                             function(x) as.numeric(x!="0")))
# tech field
no_col <- ncol(quality_predict)
quality_predict <- unique(merge(x=quality_predict, y=OECD_PATENT_QUALITY_EPO[,c(1, 4)], 
                                by= 'appln_id', all.x = TRUE))
no_add_col <- length(levels(factor(quality_predict$tech_field)))
quality_predict <- quality_predict %>% 
  pivot_wider(names_from = tech_field, values_from = tech_field, 
              values_fill = list(tech_field = 0))
quality_predict[, (no_col+1):(no_col+no_add_col)] <- data.frame(lapply(quality_predict[, (no_col+1):(no_col+no_add_col)],
                                                                       function(x) as.numeric(x!="0")))
for (i in (no_col+1):(no_col+no_add_col)){
  names(quality_predict)[i] <- paste('tech_field_', names(quality_predict)[i], sep="")
}
# whether the patent has multiple technology fields
quality_predict <- unique(merge(x=quality_predict, y=OECD_PATENT_QUALITY_EPO[,c(1, 5)], 
                                by= 'appln_id', all.x = TRUE))
# IPC
EPO_IPC$IPC3 <- substr(EPO_IPC$IPC,1,3)
no_col <- ncol(quality_predict)
quality_predict <- merge(x=quality_predict, y=unique(EPO_IPC[,c('appln_id', 'IPC3')]), 
                         by='appln_id', all.x = TRUE)
no_add_col <- length(levels(factor(quality_predict$IPC3)))
quality_predict$IPC3 <- as.character(quality_predict$IPC3)
quality_predict <- quality_predict %>% 
  pivot_wider(names_from = IPC3, values_from = IPC3, 
              values_fill = list(IPC3 = "0"))
quality_predict[, (no_col+1):(no_col+no_add_col)] <- data.frame(lapply(quality_predict[, (no_col+1):(no_col+no_add_col)], 
                                             function(x) as.numeric(x!="0")))
quality_predict <- aggregate(.~appln_id+quality_group, data=quality_predict, sum) 
not_scaled <- ncol(quality_predict)+2 # store the final column of features without the need of scaling
print(not_scaled)
# number of backward citations and other quality indicators
quality_predict <- unique(merge(x=quality_predict, y=OECD_PATENT_QUALITY_EPO[,c(20, 21, 1, 6, 7, 8, 9, 10, 11)], 
                                by= 'appln_id', all.x = TRUE))
# priority year
quality_predict <- merge(x=quality_predict, y=unique(EPO_IPC[,c(1,2)]), 
                         by='appln_id', all.x = TRUE)
# number of countries
quality_predict <- quality_predict %>% mutate(no_ctry = rowSums(.[3:12]))
quality_predict[3:(no_col+no_add_col)] <- lapply(quality_predict[3:(no_col+no_add_col)], factor)
# number of inventors
inventor <- EPO_Inv_reg[EPO_Inv_reg$appln_id %in% quality_predict$appln_id,c("appln_id", "person_id", "inv_share")] # inner join table in case of missing data
no_inventor <- aggregate(inventor$person_id, by=list(inventor$appln_id), FUN=length)
names(no_inventor) <- c('appln_id', 'no_inv')
quality_predict <- merge(x=quality_predict, y=no_inventor, by='appln_id', all.x = TRUE)
# number of applicants
system.time(EPO_App_reg <- read_delim("REGPAT/202001_EPO_App_reg.txt","|", 
                                      escape_double = FALSE, trim_ws = TRUE))
applicant <- EPO_App_reg[EPO_App_reg$appln_id %in% quality_predict$appln_id, c("appln_id", "person_id", "app_share")]
no_applicant <- aggregate(applicant$person_id, by=list(applicant$appln_id),FUN=length)
names(no_applicant) <- c('appln_id', 'no_app')
quality_predict <- merge(x=quality_predict, y=no_applicant, by='appln_id', all.x = TRUE)
# change quality groups into 0, 1, 2, 3, 4, 5
for (i in 1:length(levels(factor(quality_predict$quality_group)))){
  quality_predict$quality_group[quality_predict$quality_group==i] <- i-1
}
# remove missing data
quality_predict <- quality_predict[complete.cases(quality_predict),]
write_csv(quality_predict,"quality_predict.csv")
write_csv(inventor, "inventor.csv")
write_csv(applicant,"applicant.csv")
detach(package:plyr)    
library(dplyr)
quality_predict_year <- read_csv("quality_predict.csv")
inventor <- read_csv("inventor.csv")
applicant <- read_csv("applicant.csv")
# sum of appln_ids of all inventors for each appln_id until its prio_year
inventor_year <- merge(x=inventor[,1:2],y=quality_predict[,c("appln_id","prio_year")],
                       by="appln_id",all.y = T)
inventor_no_app <- unique(EPO_Inv_reg[EPO_Inv_reg$person_id %in% inventor_year[,2],c(2,4)])
inventor_no_app_year <- merge(x=inventor_no_app, y=unique(EPO_IPC[,c(1,2)]), by="appln_id", all.x = T)
inventor_no_app_year_agg <- aggregate(inventor_no_app_year$appln_id, 
                                      by=list(inventor_no_app_year$person_id, inventor_no_app_year$prio_year), 
                                      FUN=length)
colnames(inventor_no_app_year_agg) <- c("person_id", "prio_year", "no_app")
inventor_no_app_year_agg <-  inventor_no_app_year_agg %>% group_by(person_id) %>% mutate(cumsum = cumsum(no_app))
colnames(inventor_year)[3] <- "prio_year"
inventor_year <- merge(x=inventor_year, y=inventor_no_app_year_agg[,c(1,2,4)], 
                       by=c("person_id", "prio_year"),
                       all.x = T)
appln_id_capp <- aggregate(inventor_year$cumsum, by=list(inventor_year$appln_id), FUN=sum)
colnames(appln_id_capp) <- c("appln_id", "sum_no_app_inv")
quality_predict <- merge(x=quality_predict, y=appln_id_capp, by="appln_id", all.x=T)
# sum of appln_ids of all applicants for each appln_id until its prio_year
applicant_year <- merge(x=applicant[,1:2],y=quality_predict[,c("appln_id","prio_year")],
                        by="appln_id",all.y = T)
applicant_no_app <- unique(EPO_App_reg[EPO_App_reg$person_id %in% applicant_year[,2],c(2,4)])
applicant_no_app_year <- merge(x=applicant_no_app, y=unique(EPO_IPC[,c(1,2)]), by="appln_id", all.x = T)
applicant_no_app_year_agg <- aggregate(applicant_no_app_year$appln_id, 
                                       by=list(applicant_no_app_year$person_id, applicant_no_app_year$prio_year), 
                                       FUN=length)
colnames(applicant_no_app_year_agg) <- c("person_id", "prio_year", "no_app")
applicant_no_app_year_agg <-  applicant_no_app_year_agg %>% group_by(person_id) %>% mutate(cumsum = cumsum(no_app))
colnames(applicant_year)[3] <- "prio_year"
applicant_year <- merge(x=applicant_year, y=applicant_no_app_year_agg[,c(1,2,4)], 
                        by=c("person_id", "prio_year"),
                        all.x = T)
appln_id_capp2 <- aggregate(applicant_year$cumsum, by=list(applicant_year$appln_id), FUN=sum)
colnames(appln_id_capp2) <- c("appln_id", "sum_no_app_app")
quality_predict <- merge(x=quality_predict, y=appln_id_capp2, by="appln_id", all.x=T)
# number of experienced inventors
inventor_year_agg <- aggregate(inventor_no_app_year$prio_year, 
                               by=list(inventor_no_app_year$person_id),FUN=min)
colnames(inventor_year_agg) <- c("person_id", "prio_year")
inventor_year_agg$new <- 0
inventor_year_agg <- merge(x=inventor_no_app_year, y=inventor_year_agg, 
                           by=c("person_id", "prio_year"), all.x = T)
inventor_year_agg[is.na(inventor_year_agg)] <- 1
inventor_year_agg <- aggregate(inventor_year_agg$new, by=list(inventor_year_agg$appln_id), FUN=sum)
colnames(inventor_year_agg) <- c("appln_id", "no_exp_inv")
quality_predict <- merge(x=quality_predict, y=inventor_year_agg, by="appln_id", all.x=T)
# number of experienced applicants
applicant_year_agg <- aggregate(applicant_no_app_year$prio_year, 
                               by=list(applicant_no_app_year$person_id),FUN=min)
colnames(applicant_year_agg) <- c("person_id", "prio_year")
applicant_year_agg$new <- 0
applicant_year_agg <- merge(x=applicant_no_app_year, y=applicant_year_agg, 
                           by=c("person_id", "prio_year"), all.x = T)
applicant_year_agg[is.na(applicant_year_agg)] <- 1
applicant_year_agg <- aggregate(applicant_year_agg$new, by=list(applicant_year_agg$appln_id), FUN=sum)
colnames(applicant_year_agg) <- c("appln_id", "no_exp_app")
quality_predict <- merge(x=quality_predict, y=applicant_year_agg, by="appln_id", all.x=T)
quality_predict <- quality_predict[complete.cases(quality_predict),]
write_csv(quality_predict,"quality_predict.csv")









#------------------------------------------------------------------------------------------------









### Logistic regression with 5 groups of quality
# split data into training, cross-validation and test sets 
quality_predict <- read_csv("quality_predict.csv")
quality_predict$appln_id <- NULL
set.seed(123)
spec = c(train = .9, test = .05, validate = .05)
g = sample(cut(seq(nrow(quality_predict)), 
               nrow(quality_predict)*cumsum(c(0,spec)),
               labels = names(spec)))
use = split(quality_predict, g)
# check the size of sets
sapply(use, nrow)/nrow(quality_predict)
par(mfrow = c(1, 3)) 
plot(density(use$train$quality_group))
plot(density(use$validate$quality_group))
plot(density(use$test$quality_group))
quality_predict <- use
# train the model
model_5 <- multinom(quality_group ~ ., data=quality_predict$train, maxit=3000, MaxNWts = 84581)
# summary(model)
# Predicting the Train set results
quality_group_pred = predict(model_5, quality_predict$train[-1], decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(quality_predict$train[1], quality_group_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_5levels_train <- sum(cm$compare)/length(cm$compare) 
cm_wrong<- cm[cm$compare==0,]
cm_wrong$compare <- NULL
cm_wrong_agg <- aggregate(cm_wrong$obs, by=list(cm_wrong$obs, cm_wrong$predicted), FUN=length)
cm_wrong_agg <- cm_wrong_agg %>% unite(Group, Group.1, Group.2, sep = '') %>% rowwise() %>% mutate(Group = paste(paste(sort(unlist(strsplit(Group, "", fixed = TRUE)))), collapse=''))
cm_wrong_agg <- aggregate(cm_wrong_agg$x, by=list(cm_wrong_agg$Group), FUN=sum)
# 0 - 1 and 3-4 are often mispredicted  -> combine 0 and 1 into 1 group and 3-4 into 1 group
# Predicting the Test set results
quality_group_pred = predict(model_5, quality_predict$test[-1], decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(quality_predict$test[1], quality_group_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_5levels_test <- sum(cm$compare)/length(cm$compare) 
# Predicting the CV set results
quality_group_pred = predict(model_5, quality_predict$validate[-1], decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(quality_predict$validate[,1], quality_group_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_5levels_validate <- sum(cm$compare)/length(cm$compare) 

# save model
saveRDS(model_5, "model_5.rds")

# caculate average F1-score
a <- cbind(quality_predict$validate[,1], quality_group_pred)

a0 <- a 
a0$quality_group <- ifelse(a0$quality_group==0,1,0)
a0$quality_group_pred <- ifelse(a0$quality_group_pred==0,1,0)
precision_a0 <- posPredValue(as.factor(a0$quality_group_pred), as.factor(a0$quality_group), positive="1")
recall_a0 <- sensitivity(as.factor(a0$quality_group_pred), as.factor(a0$quality_group),, positive="1")
F1_validate_a0 <- (2 * precision_a0 * recall_a0) / (precision_a0 + recall_a0)


a1 <- a 
a1$quality_group <- ifelse(a1$quality_group==1,1,0)
a1$quality_group_pred <- ifelse(a1$quality_group_pred==1,1,0)
precision_a1 <- posPredValue(as.factor(a1$quality_group_pred), as.factor(a1$quality_group), positive="1")
recall_a1 <- sensitivity(as.factor(a1$quality_group_pred), as.factor(a1$quality_group),, positive="1")
F1_validate_a1 <- (2 * precision_a1 * recall_a1) / (precision_a1 + recall_a1)

a2 <- a 
a2$quality_group <- ifelse(a2$quality_group==2,1,0)
a2$quality_group_pred <- ifelse(a2$quality_group_pred==2,1,0)
precision_a2 <- posPredValue(as.factor(a2$quality_group_pred), as.factor(a2$quality_group), positive="1")
recall_a2 <- sensitivity(as.factor(a2$quality_group_pred), as.factor(a2$quality_group),, positive="1")
F1_validate_a2 <- (2 * precision_a2 * recall_a2) / (precision_a2 + recall_a2)

a3 <- a 
a3$quality_group <- ifelse(a3$quality_group==3,1,0)
a3$quality_group_pred <- ifelse(a3$quality_group_pred==3,1,0)
precision_a3 <- posPredValue(as.factor(a3$quality_group_pred), as.factor(a3$quality_group), positive="1")
recall_a3 <- sensitivity(as.factor(a3$quality_group_pred), as.factor(a3$quality_group),, positive="1")
F1_validate_a3 <- (2 * precision_a3 * recall_a3) / (precision_a3 + recall_a3)

a4 <- a 
a4$quality_group <- ifelse(a4$quality_group==4,1,0)
a4$quality_group_pred <- ifelse(a4$quality_group_pred==4,1,0)
precision_a4 <- posPredValue(as.factor(a4$quality_group_pred), as.factor(a4$quality_group), positive="1")
recall_a4 <- sensitivity(as.factor(a4$quality_group_pred), as.factor(a4$quality_group),, positive="1")
F1_validate_a4 <- (2 * precision_a4 * recall_a4) / (precision_a4 + recall_a4)

F1_validate_5 <- mean(c(F1_validate_a0, F1_validate_a1, F1_validate_a2, F1_validate_a3, F1_validate_a4))


#-----------------------------------------------------------------------------------------------------------


### Logistic regression of 3 groups of quality
quality_predict <- read_csv("quality_predict.csv")
quality_indicators <- read_csv("quality_indicators")


quality_predict$quality_group[quality_predict$quality_group==2] <- 0
quality_predict$quality_group[quality_predict$quality_group==3] <- 2
quality_predict$quality_group[quality_predict$quality_group==4] <- 0

# plot groups of quality according to quality_index_4 and renewal_scaled
quality_indicators <- merge(x=quality_indicators[, c(1,3,4)], 
                            y=quality_predict[,c("appln_id", "quality_group")],
                            by="appln_id", all.y = T)
par(mfrow=c(1,1))
plot(quality_indicators$quality_index_4, quality_indicators$renewal_scaled ,
     col = factor(quality_indicators$quality_group),
     xlab='Patent quality index 4',
     ylab='Number of renewals (scaled)')
mtext("Patent Quality", outer=TRUE,  cex=1, line=-1.5)



quality_plot <- merge(x=quality_output[, c(1,3,4)], y=quality_predict[, c(1,2)], by='appln_id', all.y=TRUE)
par(mfrow=c(1,1))
plot(quality_plot[, c(2,3)], col=quality_plot$quality_group)

# split data into training, cross-validation, test sets according to ratio 9:0.5:0.5
quality_predict$appln_id <- NULL
set.seed(123)
spec = c(train = .9, test = .05, validate = .05)
g = sample(cut(seq(nrow(quality_predict)), 
               nrow(quality_predict)*cumsum(c(0,spec)),
               labels = names(spec)))
use = split(quality_predict, g)
# check the size of sets
sapply(use, nrow)/nrow(quality_predict)
par(mfrow = c(1, 3)) 
plot(density(use$train$quality_group))
plot(density(use$validate$quality_group))
plot(density(use$test$quality_group))
quality_predict <- use
# train the model
model_3 <- multinom(quality_group ~ ., data=quality_predict$train, maxit=3000, MaxNWts = 84581)
# summary(model)
# Predicting the Train set results
quality_group_pred = predict(model_3, quality_predict$train[-1], decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(quality_predict$train[,1], quality_group_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_3levels_train <- sum(cm$compare)/length(cm$compare) 
cm_wrong<- cm[cm$compare==0,]
cm_wrong$compare <- NULL
cm_wrong_agg <- aggregate(cm_wrong$obs, by=list(cm_wrong$obs, cm_wrong$predicted), FUN=length)
cm_wrong_agg <- cm_wrong_agg %>% unite(Group, Group.1, Group.2, sep = '') %>% rowwise() %>%
  mutate(Group = paste(paste(sort(unlist(strsplit(Group, "", fixed = TRUE)))), collapse=''))
cm_wrong_agg <- aggregate(cm_wrong_agg$x, by=list(cm_wrong_agg$Group), FUN=sum)
# misprediction is not significant
# Predicting the Test set results
quality_group_pred = predict(model_3, quality_predict$test[-1], decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(quality_predict$test[,1], quality_group_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_3levels_test <- sum(cm$compare)/length(cm$compare) 
# Predicting the CV set results
quality_group_pred = predict(model_3, quality_predict$validate[-1], decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(quality_predict$validate[,1], quality_group_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_3levels_validate <- sum(cm$compare)/length(cm$compare) 

# save model
saveRDS(model_3, "model_3.rds")

# caculate average F1-score
a <- cbind(quality_predict$validate[,1], quality_group_pred)

a0 <- a 
a0$quality_group <- ifelse(a0$quality_group==0,1,0)
a0$quality_group_pred <- ifelse(a0$quality_group_pred==0,1,0)
precision_a0 <- posPredValue(as.factor(a0$quality_group_pred), as.factor(a0$quality_group), positive="1")
recall_a0 <- sensitivity(as.factor(a0$quality_group_pred), as.factor(a0$quality_group),, positive="1")
F1_validate_a0 <- (2 * precision_a0 * recall_a0) / (precision_a0 + recall_a0)


a1 <- a 
a1$quality_group <- ifelse(a1$quality_group==1,1,0)
a1$quality_group_pred <- ifelse(a1$quality_group_pred==1,1,0)
precision_a1 <- posPredValue(as.factor(a1$quality_group_pred), as.factor(a1$quality_group), positive="1")
recall_a1 <- sensitivity(as.factor(a1$quality_group_pred), as.factor(a1$quality_group),, positive="1")
F1_validate_a1 <- (2 * precision_a1 * recall_a1) / (precision_a1 + recall_a1)

a2 <- a 
a2$quality_group <- ifelse(a2$quality_group==2,1,0)
a2$quality_group_pred <- ifelse(a2$quality_group_pred==2,1,0)
precision_a2 <- posPredValue(as.factor(a2$quality_group_pred), as.factor(a2$quality_group), positive="1")
recall_a2 <- sensitivity(as.factor(a2$quality_group_pred), as.factor(a2$quality_group),, positive="1")
F1_validate_a2 <- (2 * precision_a2 * recall_a2) / (precision_a2 + recall_a2)

F1_validate_3 <- mean(c(F1_validate_a0, F1_validate_a1, F1_validate_a2))


#-----------------------------------------------------------------------------------------------


### Logistic regression of 2 groups of quality
quality_predict <- read_csv("quality_predict.csv")
quality_indicators <- read_csv("quality_indicators")

quality_predict$quality_group[quality_predict$quality_group==1] <- 0
quality_predict$quality_group[quality_predict$quality_group==2] <- 0
quality_predict$quality_group[quality_predict$quality_group==3] <- 1
quality_predict$quality_group[quality_predict$quality_group==4] <- 0

# plot groups of quality according to quality_index_4 and renewal_scaled
quality_indicators <- merge(x=quality_indicators[, c(1,3,4)], 
                            y=quality_predict[,c("appln_id", "quality_group")],
                            by="appln_id", all.y = T)
par(mfrow=c(1,1))
plot(quality_indicators$quality_index_4, quality_indicators$renewal_scaled ,
     col = factor(quality_indicators$quality_group),
     xlab='Patent quality index 4',
     ylab='Number of renewals (scaled)')
mtext("Patent Quality", outer=TRUE,  cex=1, line=-1.5)



quality_plot <- merge(x=quality_output[, c(1,3,4)], y=quality_predict[, c(1,2)], by='appln_id', all.y=TRUE)
par(mfrow=c(1,1))
plot(quality_plot[, c(2,3)], col=quality_plot$quality_group)

### Logistic Regression 2 levels
# split data into training, cross-validation, test sets according to ratio 9:0.5:0.5
quality_predict$appln_id <- NULL
set.seed(123)
spec = c(train = .9, test = .05, validate = .05)
g = sample(cut(seq(nrow(quality_predict)), 
               nrow(quality_predict)*cumsum(c(0,spec)),
               labels = names(spec)))
use = split(quality_predict, g)
# check the size of sets
sapply(use, nrow)/nrow(quality_predict)
par(mfrow = c(1, 3)) 
plot(density(use$train$quality_group))
plot(density(use$validate$quality_group))
plot(density(use$test$quality_group))
quality_predict <- use
# train the model
model_2 <- multinom(quality_group ~ ., data=quality_predict$train, maxit=3000, MaxNWts = 84581)
# summary(model)
# Predicting the Train set results
quality_group_pred = predict(model_2, quality_predict$train[-1], decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(quality_predict$train[,1], quality_group_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_2levels_train <- sum(cm$compare)/length(cm$compare) 
cm_wrong<- cm[cm$compare==0,]
cm_wrong$compare <- NULL
cm_wrong_agg <- aggregate(cm_wrong$obs, by=list(cm_wrong$obs, cm_wrong$predicted), FUN=length)
cm_wrong_agg <- cm_wrong_agg %>% unite(Group, Group.1, Group.2, sep = '') %>% rowwise() %>%
  mutate(Group = paste(paste(sort(unlist(strsplit(Group, "", fixed = TRUE)))), collapse=''))
cm_wrong_agg <- aggregate(cm_wrong_agg$x, by=list(cm_wrong_agg$Group), FUN=sum)
# misprediction is not significant
# Predicting the Test set results
quality_group_pred = predict(model_2, quality_predict$test[-1], decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(quality_predict$test[,1], quality_group_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_2levels_test <- sum(cm$compare)/length(cm$compare) 
# Predicting the CV set results
quality_group_pred = predict(model_2, quality_predict$validate[-1], decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(quality_predict$validate[,1], quality_group_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_2levels_validate <- sum(cm$compare)/length(cm$compare) 

# save model
saveRDS(model_2, "model_2.rds")

# caculate F1-score
precision <- posPredValue(as.factor(quality_group_pred), as.factor(quality_predict$validate$quality_group), positive="1")
recall <- sensitivity(as.factor(quality_group_pred), as.factor(quality_predict$validate$quality_group),, positive="1")
F1_validate_2 <- (2 * precision * recall) / (precision + recall)


#-----------------------------------------------------------------------------------------------


### SVM 5 levels
quality_predict <- read_csv("quality_predict.csv")
# split data into training, cross-validation, test sets
quality_predict$appln_id <- NULL
set.seed(123)
spec = c(train = .9, test = .05, validate = .05)
g = sample(cut(seq(nrow(quality_predict)), 
               nrow(quality_predict)*cumsum(c(0,spec)),
               labels = names(spec)))
use = split(quality_predict, g)
# check the size of sets
sapply(use, nrow)/nrow(quality_predict)
par(mfrow = c(1, 3)) 
plot(density(use$train$quality_group))
plot(density(use$validate$quality_group))
plot(density(use$test$quality_group))
quality_predict <- use
# SVM
model_5_svm <- svm(quality_group ~., data=quality_predict$train, 
            type= 'C-classification', kernel= 'radial', gamma=0.1, cost=10)
# Predicting the Train set results
quality_group_pred = predict(model_5_svm, quality_predict$train[-1], decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(quality_predict$train[,1], quality_group_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_5levels_train_svm <- sum(cm$compare)/length(cm$compare) 
cm_wrong<- cm[cm$compare==0,]
cm_wrong$compare <- NULL
cm_wrong_agg <- aggregate(cm_wrong$obs, by=list(cm_wrong$obs, cm_wrong$predicted), FUN=length)
write_csv(cm_wrong_agg, "cm_wrong_agg_svm5.csv")
# Predicting the Test set results
quality_group_pred = predict(model_5_svm, quality_predict$test[-1], decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(quality_predict$test[,1], quality_group_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_5levels_test_svm <- sum(cm$compare)/length(cm$compare) 
# Predicting the CV set results
quality_group_pred = predict(model_5_svm, quality_predict$validate[-1], decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(quality_predict$validate[,1], quality_group_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_5levels_validate_svm <- sum(cm$compare)/length(cm$compare) 

# save model
saveRDS(model_5_svm, "model_5_svm.rds")

# caculate average F1-score
a <- cbind(quality_predict$validate[,1], quality_group_pred)

a0 <- a 
a0$quality_group <- ifelse(a0$quality_group==0,1,0)
a0$quality_group_pred <- ifelse(a0$quality_group_pred==0,1,0)
precision_a0 <- posPredValue(as.factor(a0$quality_group_pred), as.factor(a0$quality_group), positive="1")
recall_a0 <- sensitivity(as.factor(a0$quality_group_pred), as.factor(a0$quality_group),, positive="1")
F1_validate_a0 <- (2 * precision_a0 * recall_a0) / (precision_a0 + recall_a0)


a1 <- a 
a1$quality_group <- ifelse(a1$quality_group==1,1,0)
a1$quality_group_pred <- ifelse(a1$quality_group_pred==1,1,0)
precision_a1 <- posPredValue(as.factor(a1$quality_group_pred), as.factor(a1$quality_group), positive="1")
recall_a1 <- sensitivity(as.factor(a1$quality_group_pred), as.factor(a1$quality_group),, positive="1")
F1_validate_a1 <- (2 * precision_a1 * recall_a1) / (precision_a1 + recall_a1)

a2 <- a 
a2$quality_group <- ifelse(a2$quality_group==2,1,0)
a2$quality_group_pred <- ifelse(a2$quality_group_pred==2,1,0)
precision_a2 <- posPredValue(as.factor(a2$quality_group_pred), as.factor(a2$quality_group), positive="1")
recall_a2 <- sensitivity(as.factor(a2$quality_group_pred), as.factor(a2$quality_group),, positive="1")
F1_validate_a2 <- (2 * precision_a2 * recall_a2) / (precision_a2 + recall_a2)

a3 <- a 
a3$quality_group <- ifelse(a3$quality_group==3,1,0)
a3$quality_group_pred <- ifelse(a3$quality_group_pred==3,1,0)
precision_a3 <- posPredValue(as.factor(a3$quality_group_pred), as.factor(a3$quality_group), positive="1")
recall_a3 <- sensitivity(as.factor(a3$quality_group_pred), as.factor(a3$quality_group),, positive="1")
F1_validate_a3 <- (2 * precision_a3 * recall_a3) / (precision_a3 + recall_a3)

a4 <- a 
a4$quality_group <- ifelse(a4$quality_group==4,1,0)
a4$quality_group_pred <- ifelse(a4$quality_group_pred==4,1,0)
precision_a4 <- posPredValue(as.factor(a4$quality_group_pred), as.factor(a4$quality_group), positive="1")
recall_a4 <- sensitivity(as.factor(a4$quality_group_pred), as.factor(a4$quality_group),, positive="1")
F1_validate_a4 <- (2 * precision_a4 * recall_a4) / (precision_a4 + recall_a4)

F1_validate_a0 <- 0 # can't compute due to wrong predition
F1_validate_a3 <- 0 # can't compute due to wrong predition

F1_validate_5_svm <- mean(c(F1_validate_a0, F1_validate_a1, F1_validate_a2, F1_validate_a3, F1_validate_a4))


#--------------------------------------------------------------------------------------------


### SVM with 3 groups of quality
quality_predict <- read_csv("quality_predict.csv")
quality_indicators <- read_csv("quality_indicators")


quality_predict$quality_group[quality_predict$quality_group==2] <- 0
quality_predict$quality_group[quality_predict$quality_group==3] <- 2
quality_predict$quality_group[quality_predict$quality_group==4] <- 0

# plot groups of quality according to quality_index_4 and renewal_scaled
quality_indicators <- merge(x=quality_indicators[, c(1,3,4)], 
                            y=quality_predict[,c("appln_id", "quality_group")],
                            by="appln_id", all.y = T)
par(mfrow=c(1,1))
plot(quality_indicators$quality_index_4, quality_indicators$renewal_scaled ,
     col = factor(quality_indicators$quality_group),
     xlab='Patent quality index 4',
     ylab='Number of renewals (scaled)')
mtext("Patent Quality", outer=TRUE,  cex=1, line=-1.5)



quality_plot <- merge(x=quality_output[, c(1,3,4)], y=quality_predict[, c(1,2)], by='appln_id', all.y=TRUE)
par(mfrow=c(1,1))
plot(quality_plot[, c(2,3)], col=quality_plot$quality_group)

# split data into training, cross-validation, test sets according to ratio 9:0.5:0.5
quality_predict$appln_id <- NULL
set.seed(123)
spec = c(train = .9, test = .05, validate = .05)
g = sample(cut(seq(nrow(quality_predict)), 
               nrow(quality_predict)*cumsum(c(0,spec)),
               labels = names(spec)))
use = split(quality_predict, g)
# check the size of sets
sapply(use, nrow)/nrow(quality_predict)
par(mfrow = c(1, 3)) 
plot(density(use$train$quality_group))
plot(density(use$validate$quality_group))
plot(density(use$test$quality_group))
quality_predict <- use
# train the model
model_3_svm <- svm(quality_group ~., data=quality_predict$train, 
                   type= 'C-classification', kernel= 'radial', gamma=0.1, cost=10)
# Predicting the Train set results
quality_group_pred = predict(model_3_svm, quality_predict$train[-1], decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(quality_predict$train[,1], quality_group_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_3levels_train_svm <- sum(cm$compare)/length(cm$compare) 
cm_wrong<- cm[cm$compare==0,]
cm_wrong$compare <- NULL
cm_wrong_agg <- aggregate(cm_wrong$obs, by=list(cm_wrong$obs, cm_wrong$predicted), FUN=length)
write_csv(cm_wrong_agg, "cm_wrong_agg_svm3.csv")
# Predicting the Test set results
quality_group_pred = predict(model_3_svm, quality_predict$test[-1], decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(quality_predict$test[,1], quality_group_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_3levels_test_svm <- sum(cm$compare)/length(cm$compare) 
# Predicting the CV set results
quality_group_pred = predict(model_3_svm, quality_predict$validate[-1], decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(quality_predict$validate[,1], quality_group_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_3levels_validate_svm <- sum(cm$compare)/length(cm$compare) 

# save model
saveRDS(model_3_svm, "model_3_svm.rds")

# calculate average F1-score
a <- cbind(quality_predict$validate[,1], quality_group_pred)

a0 <- a 
a0$quality_group <- ifelse(a0$quality_group==0,1,0)
a0$quality_group_pred <- ifelse(a0$quality_group_pred==0,1,0)
precision_a0 <- posPredValue(as.factor(a0$quality_group_pred), as.factor(a0$quality_group), positive="1")
recall_a0 <- sensitivity(as.factor(a0$quality_group_pred), as.factor(a0$quality_group),, positive="1")
F1_validate_a0 <- (2 * precision_a0 * recall_a0) / (precision_a0 + recall_a0)


a1 <- a 
a1$quality_group <- ifelse(a1$quality_group==1,1,0)
a1$quality_group_pred <- ifelse(a1$quality_group_pred==1,1,0)
precision_a1 <- posPredValue(as.factor(a1$quality_group_pred), as.factor(a1$quality_group), positive="1")
recall_a1 <- sensitivity(as.factor(a1$quality_group_pred), as.factor(a1$quality_group),, positive="1")
F1_validate_a1 <- (2 * precision_a1 * recall_a1) / (precision_a1 + recall_a1)

a2 <- a 
a2$quality_group <- ifelse(a2$quality_group==2,1,0)
a2$quality_group_pred <- ifelse(a2$quality_group_pred==2,1,0)
precision_a2 <- posPredValue(as.factor(a2$quality_group_pred), as.factor(a2$quality_group), positive="1")
recall_a2 <- sensitivity(as.factor(a2$quality_group_pred), as.factor(a2$quality_group),, positive="1")
F1_validate_a2 <- (2 * precision_a2 * recall_a2) / (precision_a2 + recall_a2)

F1_validate_3_svm <- mean(c(F1_validate_a0, F1_validate_a1, F1_validate_a2))


#---------------------------------------------------------------------------------------------


# create df of summarizing (average) F1-scores of all above models
evaluate_quality <- data.frame(Index=c("Accuracy for Training set", "Accuracy for Test set",
                                       "Accuracy for CV set", "(Average) F1 Score"), 
                               Logistics5=c(right_percent_5levels_train,
                                            right_percent_5levels_test,
                                            right_percent_5levels_validate,
                                            F1_validate_5),
                               Logistics3=c(right_percent_3levels_train,
                                            right_percent_3levels_test,
                                            right_percent_3levels_validate,
                                            F1_validate_3),
                               Logistics2=c(right_percent_2levels_train,
                                            right_percent_2levels_test,
                                            right_percent_2levels_validate,
                                            F1_validate_2),
                               svm5=c(right_percent_5levels_train_svm,
                                      right_percent_5levels_test_svm,
                                      right_percent_5levels_validate_svm,
                                      F1_validate_5_svm),
                               svm3=c(right_percent_3levels_train_svm,
                                      right_percent_3levels_test_svm,
                                      right_percent_3levels_validate_svm,
                                      F1_validate_3_svm))
evaluate_quality[is.na(evaluate_quality)] <- 0
write_csv(evaluate_quality, "evaluate_quality.csv")


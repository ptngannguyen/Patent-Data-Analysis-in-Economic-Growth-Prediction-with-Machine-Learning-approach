library(readr)
library(data.table)
library(ggplot2)
library(reshape2)
library(extrafont)
library(tidyr)
library(e1071)
library(nnet)
library(dplyr)
library(caret)
library(plyr)
library(eurostat)
library(textclean)
library(data.table)
library(Metrics)


# chosen countries and sectors
ctry_code_use <- c("BE", "DK", "FR", "DE", "UK", "EL", "IE", "IT", "LU", "NL")
sector <- c("BES", "GOV", "HES")


### Expenditure of R&D
search <- search_eurostat(pattern="R&D expenditure")
expend <- get_eurostat(id="rd_e_gerdtot", time_format = "num")
expend <- expend[expend$sectperf %in% sector & expend$geo %in% ctry_code_use
                 & expend$unit=="MIO_PPS_KP05",]
expend_gov <- expend[expend$sectperf=="GOV",3:5]
expend_bes <- expend[expend$sectperf=="BES",3:5]
expend_hes <- expend[expend$sectperf=="HES",3:5]
colnames(expend_bes)[3] <- "expend_bes"
colnames(expend_gov)[3] <- "expend_gov"
colnames(expend_hes)[3] <- "expend_hes"



### Human Resources in R&D
search <- search_eurostat(pattern="R&D personnel")
human <- get_eurostat(id="rd_p_perslf", time_format = "num")
human <- human[human$sex=="T" &human$sectperf == "TOTAL" & human$geo %in% ctry_code_use 
              & human$prof_pos == 'TOTAL' & human$unit =="PC_ACT_FTE", 5:7]
colnames(human)[3] <- "human"


### Unemployment rate
unem_rate <- read_csv("UnemploymentRate.csv")
unem_rate <- unem_rate[unem_rate$COUNTRY_REGION %in% c("BEL", "DNK", "FRA", "DEU", "GBR", "GRC",
                                                       "IRL", "ITA", "LTU", "NLD"),3:5]
unem_rate$COUNTRY_REGION <- mgsub::mgsub(unem_rate$COUNTRY_REGION, c("BEL", "DNK", "FRA", "DEU", "GBR", "GRC",
                                               "IRL", "ITA", "LTU", "NLD"), 
                              c("BE", "DK", "FR", "DE", "UK", "EL", "IE", "IT", "LU", "NL"))
colnames(unem_rate) <- c("geo", "time", "unem_rate")


### Active population
search <- search_eurostat(pattern="active")
active_pop <- get_eurostat(id="lfst_r_lfp2act", time_format = "num")
active_pop <- active_pop[active_pop$sex=="T" & active_pop$age=="Y15-64" 
                         & active_pop$geo %in% ctry_code_use, 4:6]
colnames(active_pop)[3] <- "active_pop"



### Physical stock of R&D
rawdat <- get_eurostat(id = "rd_e_gerdtot", time_format = "num", 
                       filters = list(geo = ctry_code_use, sectperf = sector, 
                                      unit = "MIO_PPS_KP05"))
rawdat <- rawdat[complete.cases(rawdat),]
dat <- data.table(rawdat)
names(dat)[5] <- "rdtot"
# Set depreciation rate
d <- 0.10
# Create a variable for year order
dat[, yr.order := 1:.N, by = geo]
# Create the first rdstock index by country code
dat[, rdstock := rdtot / (lm(log(rdtot)~time)$coef[2] + d), by = geo]
# Derive following calcs from rdstock index by country code
dat[yr.order != 1L, rdstock := rdstock[yr.order - 1L] *
      (1L - d) + rdtot[yr.order - 1L], by = geo]
names(dat)[6] <- "phy_stock"
phy_stock <- data.frame(dat)
phy_stock <- phy_stock[,c(1,3,4,7)]
phy_stock <- phy_stock %>% 
  pivot_wider(names_from = sectperf, values_from = rdstock,
              values_fill = list(rdstock = 0))
for (i in 3:ncol(phy_stock)){
  names(phy_stock)[i] <- paste('rdstock_', names(phy_stock)[i], sep="")
}


### merge data
gdp_predict <- merge(x=expend_bes, y=expend_gov, by=c("geo", "time"))
gdp_predict <- merge(x=gdp_predict, y=expend_hes, by=c("geo", "time"))
gdp_predict <- merge(x=gdp_predict, y=human, by=c("geo", "time"))
gdp_predict <- merge(x=gdp_predict, y=unem_rate, by=c("geo", "time"))
gdp_predict <- merge(x=gdp_predict, y=active_pop, by=c("geo", "time"))
gdp_predict <- merge(x=gdp_predict, y=phy_stock, by=c("geo", "time"))
gdp_predict$human <- gdp_predict$human*gdp_predict$active_pop/100
time <- unique(gdp_predict$time) #1999-2018
gdp_predict_core <- gdp_predict 
write_csv(gdp_predict_core, "gdp_predict_core.csv")


### GDP
search <- search_eurostat(pattern="GDP")
gdp <- get_eurostat(id="nama_10r_2gdp", time_format = "num")
gdp <- gdp[gdp$unit=="MIO_PPS_EU27_2020" & gdp$geo %in% ctry_code_use, 2:4]
colnames(gdp)[3] <- "gdp"
write_csv(gdp, "gdp.csv")
gdp$time <- gdp$time-1 #1999-2017


### merge data
gdp_predict <- merge(x=gdp, y=gdp_predict, by=c("geo", "time"))


### change country code to be consistent with patent data
ctry_code_use_2 <- c("BE", "DK", "FR", "DE", "GB", "GR", "IS", "IT", "LU", "NL")
gdp_predict$geo <- mgsub::mgsub(gdp_predict$geo, ctry_code_use, ctry_code_use_2)
write_csv(gdp_predict, "gdp_predict.csv")



#---------------------------------------------------------------------------------------------



### take all patents for countries
# OECD EPO_Inv_reg data
system.time(EPO_Inv_reg <- read_delim("REGPAT/202001_EPO_Inv_reg.txt","|", 
                                      escape_double = FALSE, trim_ws = TRUE))
# OECD EPO_IPC data
system.time(EPO_IPC <- read_delim("REGPAT/202001_EPO_IPC.txt","|", 
                                  escape_double = FALSE, trim_ws = TRUE))
# take appln_id, inventor and country information
time <- unique(gdp_predict$time)
appln_id <- unique(EPO_IPC[EPO_IPC$prio_year %in% time, 1:2])
appln_id_inv_ctry <- EPO_Inv_reg[EPO_Inv_reg$ctry_code %in% ctry_code_use_2
                                 & EPO_Inv_reg$appln_id %in% appln_id$appln_id, c(2,4,8,10)]
appln_id_inv_ctry <- merge(x=appln_id_inv_ctry, y=appln_id, by=c("appln_id"), all.x = T)
appln_id_ctry_share <- aggregate(.~ appln_id + ctry_code + prio_year, data=appln_id_inv_ctry[,c(1,3,4,5)], FUN=sum)
write_csv(appln_id_ctry_share, "appln_id_ctry_share.csv")




#----------------------------------------------------------------------------------------------


##### Predict patent quality
### Merge features for the predicting model into 1 dataframe
appln_id_ctry_share <- read_csv("appln_id_ctry_share.csv")
# country code
quality_predict <- unique(appln_id_ctry_share[,c(1,2)])
no_col <- ncol(quality_predict)-1
no_add_col <- length(levels(factor(quality_predict$ctry_code)))
quality_predict$ctry_code <- as.character(quality_predict$ctry_code)
quality_predict <- quality_predict %>% pivot_wider(names_from = ctry_code, values_from = ctry_code, 
                                                   values_fill = list(ctry_code = "0"))
quality_predict[, (no_col+1):(no_col+no_add_col)] <- data.frame(lapply(quality_predict[, (no_col+1):(no_col+no_add_col)], 
                                                                       function(x) as.numeric(x!="0")))
# tech field
no_col <- ncol(quality_predict)
OECD_PATENT_QUALITY_EPO <- read_delim("Quality/202001_OECD_PATENT_QUALITY_EPO/202001_OECD_PATENT_QUALITY_EPO_INDIC.txt","|", 
                                      escape_double = FALSE, trim_ws = TRUE)
quality_predict <- unique(merge(x=quality_predict, y=OECD_PATENT_QUALITY_EPO[,c(1, 4)], 
                                by= 'appln_id', all.x = TRUE))
quality_predict <- quality_predict[complete.cases(quality_predict),]
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
quality_predict <- quality_predict[complete.cases(quality_predict),]
no_add_col <- length(levels(factor(quality_predict$IPC3)))
quality_predict$IPC3 <- as.character(quality_predict$IPC3)
quality_predict <- quality_predict %>% 
  pivot_wider(names_from = IPC3, values_from = IPC3, 
              values_fill = list(IPC3 = "0"))
quality_predict[, (no_col+1):(no_col+no_add_col)] <- data.frame(lapply(quality_predict[, (no_col+1):(no_col+no_add_col)], 
                                                                       function(x) as.numeric(x!="0")))
quality_predict <- aggregate(.~ appln_id,data=quality_predict,sum)
not_scaled <- ncol(quality_predict)+2 # store the final column of features without the need of scaling
print(not_scaled)
# number of backward citations and other quality indicators
quality_predict <- unique(merge(x=quality_predict, y=OECD_PATENT_QUALITY_EPO[,c(20, 21, 1, 6, 7, 8, 9, 10, 11)], 
                                by= 'appln_id', all.x = TRUE))
# prio_year
quality_predict <- merge(x=quality_predict, y=unique(appln_id_ctry_share[,c(1,3)]), by="appln_id",
                         all.x = T)
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
# remove missing data
quality_predict <- quality_predict[complete.cases(quality_predict),]
write_csv(quality_predict,"quality_predict_2.csv")
write_csv(inventor, "inventor_2.csv")
write_csv(applicant,"applicant_2.csv")
detach(package:plyr)    
library(dplyr)
quality_predict <- read_csv("quality_predict_2.csv")
inventor <- read_csv("inventor_2.csv")
applicant <- read_csv("applicant_2.csv")
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
quality_predict <- quality_predict[complete.cases(quality_predict),]
write_csv(quality_predict, "quality_predict_2.csv")
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
write_csv(quality_predict,"quality_predict_2.csv")




# Predict
quality_predict <- read_csv("quality_predict_2.csv")
model_3 <- readRDS("model_3.rds")
quality_group_pred = predict(model_3, quality_predict[,-1], decision.values = TRUE, probability = TRUE)
quality_group <- cbind(quality_predict[,1], quality_group_pred)
colnames(quality_group) <- c("appln_id", "quality_group")
write_csv(quality_group,"quality_group.csv")




#---------------------------------------------------------------------------------------------




##### Predict whether a patent has emerging or non-emerging technology
### Merge features for the predicting model into 1 dataframe
appln_id_ctry_share <- read_csv("appln_id_ctry_share.csv")
### patent applications with prior years, numbers of claims, numbers  of forward citations, numbers of backwards 
#citations made to non-patent literature
merging_tech_data <- unique(OECD_PATENT_QUALITY_EPO[OECD_PATENT_QUALITY_EPO$appln_id %in%
                                               appln_id_ctry_share$appln_id, c(1, 10, 11, 9)])
## Calculate Technology cycle time (TCT)
# EPO_CITATIONS data
system.time(EPO_CITATIONS <- read_delim("Citation/202001_EPO_CITATIONS.txt","|",
                                        escape_double = FALSE, trim_ws = TRUE))
Citations <- unique(EPO_CITATIONS[EPO_CITATIONS$Cited_Appln_id %in% merging_tech_data$appln_id, 
                                  c("Cited_Appln_id","Citing_appln_id")])
# Change the names of columns 
names(Citations) <- c("Cited_appln_id", "Citing_appln_id")
# Eliminate all patents that are cited by themselves
Citations <- unique(Citations[Citations$Cited_appln_id!=Citations$Citing_appln_id,])
# We use right outer join table to make sure that the final dataframe will 
# include EVEN the stem patents which are not cited
tct_data <- merge(x=Citations, y=merging_tech_data[, c(1,2)], by.x='Cited_appln_id',
                  by.y='appln_id', all.y= T)
colnames(tct_data)[3] <- 'Cited_prio_year'
tct_data <- merge(x=tct_data, y=unique(EPO_IPC[, c(1, 2)]), by.x='Citing_appln_id', 
                  by.y='appln_id', all.x = TRUE)
colnames(tct_data)[4] <- 'Citing_prio_year'
tct_data <- tct_data[tct_data$Cited_prio_year <= tct_data$Citing_prio_year,]
# Reorder the columns in Data_1
tct_data <- tct_data[,c("Cited_appln_id", "Cited_prio_year", "Citing_appln_id", 
                        "Citing_prio_year")]
# Take patents which are cited by at least another patent
tct_data <- tct_data[complete.cases(tct_data),] 
# Time difference between prior years of cited and citing patents
tct_data$time_diff <- tct_data$Citing_prio_year-tct_data$Cited_prio_year
time_diff_median <- aggregate(tct_data$time_diff, by= list(tct_data$Cited_appln_id), FUN=median) 
colnames(time_diff_median) <- c('appln_id', 'tct')
# merge into merging_tech_data
merging_tech_data <- merge(x=merging_tech_data, y=time_diff_median, by='appln_id', all.x = TRUE)


## calculate Cited Technology Similarity Index (CTSI)
Citations <- unique(EPO_CITATIONS[EPO_CITATIONS$Citing_appln_id %in% merging_tech_data$appln_id, 
                                  c("Citing_appln_id","Cited_Appln_id")])
# Change the names of columns 
names(Citations) <- c("appln_id", "Cited_appln_id")
# Eliminate all patents that are cited by themselves
Citations <- unique(Citations[Citations$Cited_appln_id!=Citations$appln_id,])
# take IPC information
appln_ipc <- unique(EPO_IPC[EPO_IPC$appln_id %in% merging_tech_data$appln_id, c(1, 4)])
ctsi_data <- merge(x=Citations, y=appln_ipc, by='appln_id', all = TRUE) 
colnames(ctsi_data)[ncol(ctsi_data)] <- 'Citing_IPC'
appln_ipc <- unique(EPO_IPC[EPO_IPC$appln_id %in% ctsi_data$Cited_appln_id, c(1, 4)])
ctsi_data <- merge(x=ctsi_data, y=appln_ipc, by.x='Cited_appln_id', by.y = 'appln_id', all = TRUE) 
colnames(ctsi_data)[ncol(ctsi_data)] <- 'Cited_IPC'
# remove missing and uncomplete data
ctsi_data <- ctsi_data[complete.cases(ctsi_data),]
ctsi_data <- ctsi_data[nchar(ctsi_data$Citing_IPC) >= 10 & nchar(ctsi_data$Cited_IPC) >= 10,]
# divide IPC information
ctsi_data$Citing_IPC <- gsub(' ','',ctsi_data$Citing_IPC)
ctsi_data$Cited_IPC <- gsub(' ','',ctsi_data$Cited_IPC)
ctsi_data$Citing_IPC_session <- substring(ctsi_data$Citing_IPC, 1, 1)
ctsi_data$Citing_IPC_class <- substring(ctsi_data$Citing_IPC, 2, 3)
ctsi_data$Citing_IPC_subclass <- substring(ctsi_data$Citing_IPC, 4, 4)
ctsi_data$Citing_IPC_maingroup <- sub('/.*', '', substring(ctsi_data$Citing_IPC, 5))
ctsi_data$Citing_IPC_subgroup <- sub('.*/', '', ctsi_data$Citing_IPC)
ctsi_data$Cited_IPC_session <- substring(ctsi_data$Cited_IPC, 1, 1)
ctsi_data$Cited_IPC_class <- substring(ctsi_data$Cited_IPC, 2, 3)
ctsi_data$Cited_IPC_subclass <- substring(ctsi_data$Cited_IPC, 4, 4)
ctsi_data$Cited_IPC_maingroup <- sub('/.*', '', substring(ctsi_data$Cited_IPC, 5))
ctsi_data$Cited_IPC_subgroup <- sub('.*/', '', ctsi_data$Cited_IPC)
# calculate ctsi
ctsi_data$cs <- (ctsi_data$Citing_IPC_session==ctsi_data$Cited_IPC_session) +
  (ctsi_data$Citing_IPC_class == ctsi_data$Cited_IPC_class) + 
  (ctsi_data$Citing_IPC_subclass == ctsi_data$Cited_IPC_subclass) +
  (ctsi_data$Citing_IPC_maingroup == ctsi_data$Cited_IPC_maingroup) +
  (ctsi_data$Citing_IPC_subgroup == ctsi_data$Cited_IPC_subgroup)
ctsi <- aggregate(ctsi_data$cs, by=list(ctsi_data$appln_id, ctsi_data$Cited_appln_id), FUN=sum)
colnames(ctsi) <- c('appln_id', 'Cited_appln_id', 'cs')
ctsi_1 <- ddply(ctsi_data,~appln_id,summarise,no_ipc=length(unique(Citing_IPC)))
ctsi_2 <- ddply(ctsi_data,~Cited_appln_id,summarise,no_cited_ipc=length(unique(Cited_IPC)))
ctsi <- merge(x=ctsi, y=ctsi_1, by='appln_id', all.x = TRUE)
ctsi <- merge(x=ctsi, y=ctsi_2, by='Cited_appln_id', all.x = TRUE)
ctsi$pcs <- ctsi$cs/(ctsi$no_ipc*ctsi$no_cited_ipc)
ctsi_3 <- ddply(ctsi_data,~appln_id,summarise,no_cited_patent=length(unique(Cited_appln_id)))
ctsi_4 <- aggregate(ctsi$pcs, by=list(ctsi$appln_id), FUN=sum)
colnames(ctsi_4) <- c('appln_id', 'total_pcs')
ctsi_3 <- merge(x=ctsi_3, y= ctsi_4, by='appln_id', all.x = TRUE)
ctsi_3$ctsi <- ctsi_3$total_pcs/ctsi_3$no_cited_patent
merging_tech_data <- merge(x=merging_tech_data, y=ctsi_3[, c(1,4)], by='appln_id', all.x = TRUE)

# calculate Cited patents Assignee Similarity Index (CASI)
system.time(EPO_App_reg <- read_delim("REGPAT/202001_EPO_App_reg.txt","|",
                                      escape_double = FALSE, trim_ws = TRUE))
Assignees <- unique(EPO_App_reg[EPO_App_reg$appln_id %in% merging_tech_data$appln_id, 
                                c("appln_id","person_id")])
casi_data <- merge(x=Citations, y=Assignees, by='appln_id', all.y = TRUE)
Citing_no_assignee <- aggregate(Assignees$person_id, by=list(Assignees$appln_id), FUN=length)
colnames(Citing_no_assignee) <- c('appln_id', 'no_citing_assignee')
Assignees <- unique(EPO_App_reg[EPO_App_reg$appln_id %in% casi_data$Cited_appln_id, 
                                c("appln_id","person_id")])
colnames(Assignees) <- c('Cited_appln_id', 'Cited_person_id')
casi_data <- merge(x=casi_data, y=Assignees, by='Cited_appln_id', all = TRUE)
casi_data$asgasg <- ifelse(casi_data$person_id==casi_data$Cited_person_id, 1, 0)
casi <- aggregate(casi_data$asgasg, by=list(casi_data$appln_id, casi_data$Cited_appln_id), 
                  FUN=sum)
colnames(casi) <- c('appln_id', 'Cited_appln_id', 'total_asgasg')
Cited_no_assignee <- aggregate(Assignees$Cited_person_id, by=list(Assignees$Cited_appln_id), FUN=length)
colnames(Cited_no_assignee) <- c('Cited_appln_id', 'no_cited_assignee')
casi <- merge(x=casi, y=Cited_no_assignee, by='Cited_appln_id', all.x = TRUE)
casi <- merge(x=casi, y=Citing_no_assignee, by='appln_id', all.x = TRUE)
casi <- casi[complete.cases(casi),]
casi$as <- casi$total_asgasg/(casi$no_cited_assignee*casi$no_citing_assignee)
casi_1 <- aggregate(casi$Cited_appln_id, by=list(casi$appln_id), FUN=length)
colnames(casi_1) <- c('appln_id', 'no_cited_patent')
casi_2 <- aggregate(casi$as, by=list(casi$appln_id), FUN=sum)
colnames(casi_2) <- c('appln_id', 'total_as')
casi_2 <- merge(x=casi_2, y=casi_1, by='appln_id', all.x = TRUE)
casi_2$casi <- casi_2$total_as/casi_2$no_cited_patent
merging_tech_data <- merge(x=merging_tech_data, y=casi_2[, c(1,4)], by='appln_id', all.x = TRUE)

merging_tech_data <- merge(x=merging_tech_data, y=appln_id_ctry_share[,c(1,3)], 
                           by="appln_id", all.x=T)
merging_tech_data <- merging_tech_data[complete.cases(merging_tech_data),]
write_csv(merging_tech_data,"merging_tech_data2.csv")


### Predict
merging_tech_data <- read_csv("merging_tech_data2.csv", col_types = cols(X = col_skip()))
model <- readRDS("model.rds")
merging_tech_data_pred = predict(model, 
                                 merging_tech_data[,-1], 
                                 decision.values = TRUE, probability = TRUE)
emerging_group <- cbind(x=merging_tech_data[,1], y=merging_tech_data_pred)
colnames(emerging_group) <- c("appln_id", "emerging_group")
write_csv(emerging_group, "emerging_group.csv")



#------------------------------------------------------------------------------------------------



### Create a dataframe to classify all patents
quality_group <- read_csv("quality_group.csv")
emerging_group <- read_csv("emerging_group.csv")
all <- merge(x=quality_group, y=emerging_group,by="appln_id", all=T)
# change labels of groups for easier understandings
# for quality groups
old <- 0:2
new <- c(1,3,2)
all$quality_group[all$quality_group %in% old] <- new[match(all$quality_group, old, nomatch = 0)]
# for EM - NEM groups
old <- c(0,1)
new <- c(1,2)
all$emerging_group[all$emerging_group %in% old] <- new[match(all$emerging_group, old, nomatch = 0)]

appln_id_ctry_share <- read_csv("appln_id_ctry_share.csv")
all <- merge(x=appln_id_ctry_share, y=all, by="appln_id", all = T)
all[is.na(all)] <- 0
all$group <- interaction(all$quality_group, all$emerging_group)
all <- unique(all[,c(1:4,7)])
all_agg <- aggregate(all$inv_share, by=list(all$ctry_code, all$prio_year, all$group), FUN=sum)
colnames(all_agg) <- c("ctry_code", "prio_year", "group", "no_appln_id")
all_agg <- all_agg %>% 
  pivot_wider(names_from = group, values_from = no_appln_id,
              values_fill = list(no_appln_id = 0))
for (i in 3:ncol(all_agg)){
  names(all_agg)[i] <- paste('appln_id_group_', names(all_agg)[i], sep="")
}
all_agg[, 3:ncol(all_agg)] <- sapply(all_agg[, 3:ncol(all_agg)], as.numeric)
write_csv(all_agg,"all_agg.csv")



#----------------------------------------------------------------------------------------------



### Data for GDP predicting models
gdp_predict <- read_csv("gdp_predict.csv")
all_agg <- read_csv("all_agg.csv")
colnames(all_agg)[c(1,2)] <- c("geo", "time") 
gdp_predict <- merge(x=gdp_predict, y=all_agg, by=c("geo", "time"), all.x = T)
write_csv(gdp_predict,"gdp_predict.csv")
geo <- gdp_predict[,1]
gdp_predict <- gdp_predict[,-1]
gdp_predict <- gdp_predict[,colSums(gdp_predict)>0]
gdp_predict <- cbind(geo, gdp_predict)
write_csv(gdp_predict,"gdp_predict_total.csv")
colnames(gdp_predict)[13:22] = gsub("appln_id_group", "gr", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("0.", "noqua_", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("0", "notech", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("1", "0", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("2", "1", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("3", "2", colnames(gdp_predict)[13:22])
write_csv(gdp_predict,"gdp_predict.csv")



#----------------------------------------------------------------------------------------------



# number of patents of each country
system.time(EPO_Inv_reg <- read_delim("REGPAT/202001_EPO_Inv_reg.txt","|", 
                                      escape_double = FALSE, trim_ws = TRUE))
system.time(EPO_IPC <- read_delim("REGPAT/202001_EPO_IPC.txt","|", 
                                  escape_double = FALSE, trim_ws = TRUE))
ctry_code_use <- c("BE", "DK", "FR", "DE", "GB", "GR", "IS", "IT", "LU", "NL")
appln_id_ctry <- unique(EPO_Inv_reg[EPO_Inv_reg$ctry_code %in% ctry_code_use, 
                                    c("appln_id", "ctry_code", "inv_share")])

### Application IDs by at least 1 inventor from 1 of 9 chosen
# EU countries from 1999 to 2017
appln_id_prio <- unique(EPO_IPC[EPO_IPC$appln_id %in% unique(appln_id_ctry$appln_id), c("appln_id", "prio_year")])
appln_id_prio_99_17 <- appln_id_prio[appln_id_prio$prio_year >= 1999 & appln_id_prio$prio_year<= 2017,]
appln_id_prio_geo <- merge(appln_id_prio_99_17, appln_id_ctry, by="appln_id", all.x = T)
no_app_geo <- aggregate(appln_id_prio_geo$inv_share, by=list(appln_id_prio_geo$ctry_code, 
                                                             appln_id_prio_geo$prio_year), FUN=sum)
colnames(no_app_geo) <- c("geo", "time", "no_patents")
write_csv(no_app_geo, "no_app_geo.csv")



#---------------------------------------------------------------------------------------------



### Predict GDP
gdp_predict <- read_csv("gdp_predict.csv")
no_app_geo <- read_csv("no_app_geo.csv")
gdp_predict <- merge(gdp_predict, no_app_geo, by=c("geo", "time"), all.x=T)
# gdp_predict$gr_0.1.1.1 <- gdp_predict$gr_0.1 +gdp_predict$gr_1.1
gdp_predict <- gdp_predict[,-c(1,2, 13:21)]
set.seed(123)
spec = c(train = .6, test = .4)
g = sample(cut(seq(nrow(gdp_predict)), 
               nrow(gdp_predict)*cumsum(c(0,spec)),
               labels = names(spec)))
use = split(gdp_predict, g)
gdp_predict <- use
model_gdp_lm <- lm(gdp~., data=gdp_predict$train)
summary(model_gdp_lm)
saveRDS(model_gdp_lm, "model_gdp_lm.RDS")

predicted <- predict(model_gdp_lm, gdp_predict$test[-1])
dat_1y <- cbind(gdp_predict$test[1], predicted, method="Predict GDP for 1 year period")
rmse_1y <- rmse(dat_1y[,1],dat_1y[,2])



#---------------------------------------------------------------------------------------------



# Predict GDP for 2 years later
gdp <- read_csv("gdp.csv")
gdp_predict_core <- read_csv("gdp_predict_core.csv")
gdp$time <- gdp$time-2 #1998-2016


### merge data
gdp_predict <- merge(x=gdp, y=gdp_predict_core, by=c("geo", "time"))


### change country code to be consistent with patent data
ctry_code_use_2 <- c("BE", "DK", "FR", "DE", "GB", "GR", "IS", "IT", "LU", "NL")
gdp_predict$geo <- mgsub::mgsub(gdp_predict$geo, ctry_code_use, ctry_code_use_2)



all_agg <- read_csv("all_agg.csv")
colnames(all_agg)[c(1,2)] <- c("geo", "time") 
gdp_predict <- merge(x=gdp_predict, y=all_agg, by=c("geo", "time"), all.x = T)
colnames(gdp_predict)[13:22] = gsub("appln_id_group", "gr", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("0.", "noqua_", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("0", "notech", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("1", "0", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("2", "1", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("3", "2", colnames(gdp_predict)[13:22])



### Predict GDP
gdp_predict <- merge(gdp_predict, no_app_geo, by=c("geo", "time"), all.x=T)
gdp_predict <- gdp_predict[,-c(1,2, 13:21)]
gdp_predict <- gdp_predict[complete.cases(gdp_predict),]
set.seed(123)
spec = c(train = .6, test = .4)
g = sample(cut(seq(nrow(gdp_predict)), 
               nrow(gdp_predict)*cumsum(c(0,spec)),
               labels = names(spec)))
use = split(gdp_predict, g)
gdp_predict <- use
model_gdp_lm_2y <- lm(gdp~., data=gdp_predict$train)
summary(model_gdp_lm_2y)
saveRDS(model_gdp_lm_2y, "model_gdp_lm_2y.RDS")


predicted <- predict(model_gdp_lm_2y, gdp_predict$test[-1])
dat_2y <- cbind(gdp_predict$test[1], predicted, method="Predict GDP for 2 year period")
rmse_2y <- rmse(dat_2y[,1],dat_2y[,2])

#---------------------------------------------------------------------------------------------




# Predict GDP for 3 years later
gdp <- read_csv("gdp.csv")
gdp_predict_core <- read_csv("gdp_predict_core.csv")
gdp$time <- gdp$time-3 #1997-2015


### merge data
gdp_predict <- merge(x=gdp, y=gdp_predict_core, by=c("geo", "time"))


### change country code to be consistent with patent data
ctry_code_use_2 <- c("BE", "DK", "FR", "DE", "GB", "GR", "IS", "IT", "LU", "NL")
gdp_predict$geo <- mgsub::mgsub(gdp_predict$geo, ctry_code_use, ctry_code_use_2)



all_agg <- read_csv("all_agg.csv")
colnames(all_agg)[c(1,2)] <- c("geo", "time") 
gdp_predict <- merge(x=gdp_predict, y=all_agg, by=c("geo", "time"), all.x = T)
colnames(gdp_predict)[13:22] = gsub("appln_id_group", "gr", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("0.", "noqua_", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("0", "notech", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("1", "0", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("2", "1", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("3", "2", colnames(gdp_predict)[13:22])



### Predict GDP
gdp_predict <- merge(gdp_predict, no_app_geo, by=c("geo", "time"), all.x=T)
gdp_predict <- gdp_predict[,-c(1,2, 13:21)]
gdp_predict <- gdp_predict[complete.cases(gdp_predict),]
set.seed(123)
spec = c(train = .6, test = .4)
g = sample(cut(seq(nrow(gdp_predict)), 
               nrow(gdp_predict)*cumsum(c(0,spec)),
               labels = names(spec)))
use = split(gdp_predict, g)
gdp_predict <- use
model_gdp_lm_3y <- lm(gdp~., data=gdp_predict$train)
summary(model_gdp_lm_3y)
saveRDS(model_gdp_lm_3y, "model_gdp_lm_3y.RDS")


predicted <- predict(model_gdp_lm_3y, gdp_predict$test[-1])
dat_3y <- cbind(gdp_predict$test[1], predicted, method="Predict GDP for 3 year period")
rmse_3y <- rmse(dat_3y[,1],dat_3y[,2])


#---------------------------------------------------------------------------------------------




# Predict GDP for 4 years later
gdp <- read_csv("gdp.csv")
gdp_predict_core <- read_csv("gdp_predict_core.csv")
gdp$time <- gdp$time-4 #1996-2014


### merge data
gdp_predict <- merge(x=gdp, y=gdp_predict_core, by=c("geo", "time"))


### change country code to be consistent with patent data
ctry_code_use_2 <- c("BE", "DK", "FR", "DE", "GB", "GR", "IS", "IT", "LU", "NL")
gdp_predict$geo <- mgsub::mgsub(gdp_predict$geo, ctry_code_use, ctry_code_use_2)



all_agg <- read_csv("all_agg.csv")
colnames(all_agg)[c(1,2)] <- c("geo", "time") 
gdp_predict <- merge(x=gdp_predict, y=all_agg, by=c("geo", "time"), all.x = T)
colnames(gdp_predict)[13:22] = gsub("appln_id_group", "gr", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("0.", "noqua_", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("0", "notech", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("1", "0", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("2", "1", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("3", "2", colnames(gdp_predict)[13:22])



### Predict GDP
gdp_predict <- merge(gdp_predict, no_app_geo, by=c("geo", "time"), all.x=T)
gdp_predict <- gdp_predict[,-c(1,2,13:21)]
gdp_predict <- gdp_predict[complete.cases(gdp_predict),]
set.seed(123)
spec = c(train = .6, test = .4)
g = sample(cut(seq(nrow(gdp_predict)), 
               nrow(gdp_predict)*cumsum(c(0,spec)),
               labels = names(spec)))
use = split(gdp_predict, g)
gdp_predict <- use
model_gdp_lm_4y <- lm(gdp~., data=gdp_predict$train)
summary(model_gdp_lm_4y)
saveRDS(model_gdp_lm_4y, "model_gdp_lm_4y.RDS")


predicted <- predict(model_gdp_lm_4y, gdp_predict$test[-1])
dat_4y <- cbind(gdp_predict$test[1], predicted, method="Predict GDP for 4 year period")
rmse_4y <- rmse(dat_4y[,1],dat_4y[,2])


#---------------------------------------------------------------------------------------------




# Predict GDP for 5 years later
gdp <- read_csv("gdp.csv")
gdp_predict_core <- read_csv("gdp_predict_core.csv")
gdp$time <- gdp$time-5 #1996-2014


### merge data
gdp_predict <- merge(x=gdp, y=gdp_predict_core, by=c("geo", "time"))


### change country code to be consistent with patent data
ctry_code_use_2 <- c("BE", "DK", "FR", "DE", "GB", "GR", "IS", "IT", "LU", "NL")
gdp_predict$geo <- mgsub::mgsub(gdp_predict$geo, ctry_code_use, ctry_code_use_2)



all_agg <- read_csv("all_agg.csv")
colnames(all_agg)[c(1,2)] <- c("geo", "time") 
gdp_predict <- merge(x=gdp_predict, y=all_agg, by=c("geo", "time"), all.x = T)
colnames(gdp_predict)[13:22] = gsub("appln_id_group", "gr", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("0.", "noqua_", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("0", "notech", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("1", "0", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("2", "1", colnames(gdp_predict)[13:22])
colnames(gdp_predict)[13:22] = gsub("3", "2", colnames(gdp_predict)[13:22])



### Predict GDP
gdp_predict <- merge(gdp_predict, no_app_geo, by=c("geo", "time"), all.x=T)
gdp_predict <- gdp_predict[,-c(1,2,13:21)]
gdp_predict <- gdp_predict[complete.cases(gdp_predict),]
set.seed(123)
spec = c(train = .6, test = .4)
g = sample(cut(seq(nrow(gdp_predict)), 
               nrow(gdp_predict)*cumsum(c(0,spec)),
               labels = names(spec)))
use = split(gdp_predict, g)
gdp_predict <- use
model_gdp_lm_5y <- lm(gdp~., data=gdp_predict$train)
summary(model_gdp_lm_5y)
saveRDS(model_gdp_lm_5y, "model_gdp_lm_5y.RDS")


predicted <- predict(model_gdp_lm_5y, gdp_predict$test[-1])
dat_5y <- cbind(gdp_predict$test[1], predicted, method="Predict GDP for 5 year period")
rmse_5y <- rmse(dat_5y[,1],dat_5y[,2])



#---------------------------------------------------------------------------------------------



options("scipen"=100, "digits"=4)
evaluate_gdp <- data.frame(Performance=c("Adjusted R-squared for Training set",
                                         "RMSE for Cross-Validation set"),
                           Predict_GDP_for_1_year_period=c(summary(model_gdp_lm)$adj.r.squared, rmse_1y),
                           Predict_GDP_for_2_year_period=c(summary(model_gdp_lm_2y)$adj.r.squared, rmse_2y),
                           Predict_GDP_for_3_year_period=c(summary(model_gdp_lm_3y)$adj.r.squared, rmse_3y),
                           Predict_GDP_for_4_year_period=c(summary(model_gdp_lm_4y)$adj.r.squared, rmse_4y),
                           Predict_GDP_for_5_year_period=c(summary(model_gdp_lm_5y)$adj.r.squared, rmse_5y))
write_csv(evaluate_gdp, "evaluate_gdp.csv")



#---------------------------------------------------------------------------------------------



### Plot for GDP prediction
dat <- rbind(dat_1y, dat_2y, dat_3y, dat_4y, dat_5y)
colnames(dat)[3] <- "model"
write_csv(dat, "dat.csv")

options("scipen"=100, "digits"=4)
dat <- read_csv("dat.csv")
ggplot(data=dat, aes(gdp)) + geom_smooth(aes(y=predicted, colour=model)) +
  labs(
    title = "Plot showing the different predictions",
    subtitle = "with smoothed lines"
  ) + geom_abline(intercept=0, slope=1)



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
library(plyr)
library(tm)
library(textmineR)
library(data.table)

### Prepare data


OECD_PATENT_QUALITY_EPO <- read_delim("Quality/202001_OECD_PATENT_QUALITY_EPO/202001_OECD_PATENT_QUALITY_EPO_INDIC.txt","|", 
                                      escape_double = FALSE, trim_ws = TRUE)
system.time(EPO_App_reg <- read_delim("REGPAT/202001_EPO_App_reg.txt","|", 
                                      escape_double = FALSE, trim_ws = TRUE))
system.time(EPO_IPC <- read_delim("REGPAT/202001_EPO_IPC.txt","|", 
                                  escape_double = FALSE, trim_ws = TRUE))
system.time(EPO_Inv_reg <- read_delim("REGPAT/202001_EPO_Inv_reg.txt","|", 
                                      escape_double = FALSE, trim_ws = TRUE))


###### CHOOSE COUNTRIES which became members of EU in 1981 or before -> more homogenerous 
ctry_code_use <- c("BE", "DK", "FR", "DE", "GB", "GR", "IS", "IT", "LU", "NL")
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



### patent applications with prior years, numbers of claims, numbers  of backward citations, numbers of backwards 
#citations made to non-patent literature
merging_tech_data <- merge(x=appln_id_prio_81_95, y=OECD_PATENT_QUALITY_EPO[, c(1, 10, 11, 9)], by='appln_id', 
                           all.x= TRUE)


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
# change the names of columns 
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
# remove missing and incomplete data
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




#-------------------------------------------------------------------------------------------------




### K-means clustering
merging_tech_data <- merging_tech_data[complete.cases(merging_tech_data),]
merging_tech_data_raw <- merging_tech_data
par(mfrow=c(2,3))
plot(density(merging_tech_data$npl_cits))
plot(density(merging_tech_data$claims))
plot(density(merging_tech_data$bwd_cits))
plot(density(merging_tech_data$tct))
plot(density(merging_tech_data$ctsi))
plot(density(merging_tech_data$casi))
# Because the features are not normal distributed, I don't use AVOVA-Test to choose 
#the number of clusters. I use Elbow Method instead. 
# feature scaling
scaling_func <- function(x){(x-min(x))/(max(x)-min(x))}
merging_tech_data$npl_cits <- scaling_func(merging_tech_data$npl_cits)
merging_tech_data$claims <- scaling_func(merging_tech_data$claims)
merging_tech_data$bwd_cits <- scaling_func(merging_tech_data$bwd_cits)
merging_tech_data$tct <- scaling_func(merging_tech_data$tct)
merging_tech_data$ctsi <- scaling_func(merging_tech_data$ctsi)
merging_tech_data$casi <- scaling_func(merging_tech_data$casi)
# change prio_year into binary features
merging_tech_data <- merging_tech_data[, c(1,3,4,5,6,7,8,2)]
no_col <- ncol(merging_tech_data)-1
no_add_col <- length(levels(factor(merging_tech_data$prio_year)))
merging_tech_data <- merging_tech_data %>% 
  pivot_wider(names_from = prio_year, values_from = prio_year, 
              values_fill = list(prio_year = 0))
merging_tech_data[, (no_col+1):(no_col+no_add_col)] <- data.frame(lapply(merging_tech_data[, (no_col+1):(no_col+no_add_col)], 
                                                                         function(x) as.numeric(x!="0")))
merging_tech_data[(no_col+1):(no_col+no_add_col)] <- lapply(merging_tech_data[(no_col+1):(no_col+no_add_col)], factor)
for (i in (no_col+1):(no_col+no_add_col)){
  names(merging_tech_data)[i] <- paste('prio_year_', names(merging_tech_data)[i], sep="")
}
merging_tech_data <- merge(x=merging_tech_data, y=merging_tech_data_raw[,c(1,2)],
                           by="appln_id", all.x = T)
merging_tech_data_copy <- merging_tech_data 
write_csv(merging_tech_data, "merging_tech_data_mini.csv")
# Try many numbers of clusters
merging_tech_data <- read_csv("merging_tech_data_mini.csv")
set.seed(123)
seed <- sample(1:200, 6, replace=FALSE)
par(mfrow = c(2, 3)) 
for (i in seed){
  set.seed(i)
  wss=vector()
  for (j in 2:20){
    merging_tech_k_means_rs <- kmeans(merging_tech_data[, -1], j)
    wss<- c(wss,merging_tech_k_means_rs$tot.withinss)
  }
  plot(2:20, wss,
       type="b", pch = 11, frame = TRUE, 
       main = paste("with set.seed(", i,")", sep=""),
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
}
mtext("K-means cluster for patents", outer=TRUE,  cex=1, line=-1.5)
# the higher the number of clusters is, the less the Total within-clusters sum of squares is
# to prepare for the text-clustering step, 20 is a good choice


# label patents with its cluster number
length(unique(merging_tech_data_raw$prio_year)) # 15
no_clusters <- 20
tech_group <- vector()

for (year in unique(merging_tech_data_raw$prio_year)){
  wss <- vector()
  for (i in seed){
    set.seed(i)
    merging_tech_k_means_rs <- kmeans(merging_tech_data[merging_tech_data$prio_year==year, 
                                                        -c(1,ncol(merging_tech_data))], no_clusters)
    wss<- c(wss,merging_tech_k_means_rs$tot.withinss)
  }
  set.seed(seed[match(min(wss), wss)])
  tech_group <- append(tech_group, 
                             kmeans(merging_tech_data[merging_tech_data$prio_year==year, 
                                                      -c(1,ncol(merging_tech_data))], no_clusters)$cluster)
}
merging_tech_data <- cbind(merging_tech_data, tech_group)
merging_tech_data$prior_year_tech_group <- paste(as.character(merging_tech_data$prio_year), 
                                                 as.character(merging_tech_data$tech_group),
                                                 sep="_")

write_csv(merging_tech_data,'merging_tech_data.csv')
appln_id_tech_group <- merging_tech_data[,c(1, ncol(merging_tech_data))] 
write_csv(appln_id_tech_group,'appln_id_tech_group.csv')




#---------------------------------------------------------------------------------------------------------




### Cluster patents based on their abstract
# import patent abstract
patent_abstract <- read_delim("Abstract/patent_abstract.csv", ";", escape_double = FALSE, trim_ws = TRUE)
appln_id_tech_group <- read_csv("appln_id_tech_group.csv")
appln_id_tech_group <- appln_id_tech_group[appln_id_tech_group$appln_id %in% patent_abstract$appln_id,]
appln_id_tech_group_agg <- aggregate(appln_id_tech_group$appln_id, by=list(appln_id_tech_group$prior_year_tech_group), FUN=length)


# Natural language processing and K-means clustering                
set.seed(123)
seed <- sample(1:200, 6, replace=FALSE)
merge_df <- data.frame(appln_id = as.numeric(), abstract_group = as.character())

for (i in (levels(as.factor(appln_id_tech_group$prior_year_tech_group)))){
  appln_id <- appln_id_tech_group[appln_id_tech_group$prior_year_tech_group==i,1]
  patent_abstract_tiny <- patent_abstract[patent_abstract$appln_id %in% appln_id$appln_id,]
  dtm <- CreateDtm(doc_vec = patent_abstract_tiny$appln_abstract, 
                   doc_names = patent_abstract_tiny$appln_id, 
                   ngram_window = c(1, 2), # minimum and maximum n-gram length
                   stopword_vec = c(stopwords::stopwords("en"),
                                    stopwords::stopwords("de"),
                                    stopwords::stopwords("fr"),
                                    stopwords::stopwords(source = "smart")), 
                   lower = TRUE, 
                   remove_punctuation = TRUE, 
                   remove_numbers = TRUE, 
                   verbose = FALSE, 
                   cpus = 2) 
  # construct the matrix of term counts to get the IDF vector
  tf_mat <- TermDocFreq(dtm)
  # TF-IDF and cosine similarity
  tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf
  tfidf <- t(tfidf)
  tfidf.matrix <- as.matrix(tfidf) 
  # K-means clustering
  wss=vector()
  no_clusters <- ifelse(nrow(appln_id)>=20,nrow(appln_id)%/%10,nrow(appln_id)%/%10+1)
  for (j in seed){
    set.seed(j)
    tfidf.matrix_k_means_rs <- kmeans(tfidf.matrix, no_clusters)
    wss<- c(wss,tfidf.matrix_k_means_rs$tot.withinss)
  }
  set.seed(seed[match(min(wss), wss)])
  appln_id <- cbind(appln_id, kmeans(tfidf.matrix, no_clusters)$cluster)
  colnames(appln_id)[2] <- 'abstract_group'
  merge_df <- rbind(merge_df,appln_id)
}

write_csv(merge_df, "merge_df.csv")
appln_id_tech_group <- merge(x = appln_id_tech_group, y = merge_df, by='appln_id', all.x = TRUE)
assign('appln_id_group', appln_id_tech_group)
appln_id_tech_group$group <- paste(as.character(appln_id_tech_group$abstract_group), 
                                         as.character(appln_id_tech_group$prior_year_tech_group), sep="_")
write_csv(appln_id_tech_group, 'appln_id_tech_group.csv')




#-------------------------------------------------------------------------------------------------------------




### take patents with new IPCs
# take all IPCs existing before 1982
EPO_IPC$IPC <- gsub(' ','', EPO_IPC$IPC)
IPC <- unique(EPO_IPC[EPO_IPC$prio_year <= 1981, 4])
IPC <- unique(substr(IPC$IPC, start = 1, stop = 3))
# take all patents from 1982 to 1996
id_IPC_af_82 <- unique(EPO_IPC[EPO_IPC$prio_year >= 1982 & EPO_IPC$prio_year <=1996, c(1,2,4)]) 
id_IPC_af_82 <- id_IPC_af_82[complete.cases(id_IPC_af_82),]
id_IPC_af_82$IPC <- substr(id_IPC_af_82$IPC, start = 1, stop = 3)
id_IPC_af_82 <- unique(id_IPC_af_82)
# take all patents with new IPCs
'%!in%' <- function(x,y)!('%in%'(x,y))
new <- data.frame(appln_id=integer(),prior_year=integer(), IPC=character()) 
year <- unique(id_IPC_af_82$prio_year)
for (i in year){
  id_IPC_af_82_sub <- id_IPC_af_82[id_IPC_af_82$prio_year == i,]
  id_IPC_af_82_sub <- id_IPC_af_82_sub[id_IPC_af_82_sub$IPC %!in% IPC,]
  IPC <- rbind(IPC, unique(id_IPC_af_82_sub$IPC))
  new <- rbind(new, id_IPC_af_82_sub)
}
write_csv(new, 'new_IPC.csv')

### take patents with new IPC combinations
df1 <- unique(EPO_IPC[EPO_IPC$prio_year <= 1981, c(1,2,4)])
df1 <- df1[complete.cases(df1),]
df1$IPC <- substr(df1$IPC, start = 1, stop = 3)
df1 <- unique(df1)
combine <- list()
year <- unique(df1$prio_year)
appln_id <- unique(df1$appln_id)
for (i in year){
  for (j in appln_id){
    combine_extend <- sort((df1[(df1$prio_year==i) & (df1$appln_id==j),3])[[1]])
    combine[[length(combine)+1]] <- combine_extend
  }
  combine <- unique(combine)
}
for (m in combine){
  for (n in combine[combine != m]){
    if (all(m %in% n) == TRUE) {
      combine <- combine[combine != m]
    }
  }
}
save(combine, file="combine2.RData") 

load("combine2.RData")
combine_2 <- combine
df2 <- unique(EPO_IPC[EPO_IPC$prio_year >= 1982
                      & EPO_IPC$prio_year <= 1996, c(1,2,4)])
df2 <- df2[complete.cases(df2),]
df2$IPC <- substr(df2$IPC, start = 1, stop = 3)
df2 <- unique(df2)
df2$usable <- TRUE
df2$grps <- interaction(df2$appln_id, df2$prio_year, drop=TRUE)
newlist <- list()
for (grp in levels(df2$grps)) {
  rows <- df2$grps == grp & df2$usable
  if (!length(rows)) next
  thisIPC <- df2$IPC[rows]
  matches <- sapply(combine_2, function(comb) all(thisIPC %in% comb))
  if (any(matches)) {
  } else {
    combine_2 <- c(combine_2, list(sort(thisIPC)))
    newlist <- c(newlist, unique(df2[rows,1]))
    df2$usable[rows] <- FALSE
  }
  print(nrow(df2[df2$usable==FALSE,]))
  if (nrow(df2[df2$usable==FALSE,])%%1000==0) {
    write_csv(df2, "df2.csv")
    save(newlist, file="newlist.RData")
    save(combine_2, file="combine2.RData") 
  }
}
df2 <- df2[df2$usable,]
new_2 <- do.call(rbind, newlist)
df2$usable <- df2$grps <- 
  new_2$usable <- new_2$grps <- NULL
save(new_2, file="new_2.RData")




#--------------------------------------------------------------------------------------------




### calculate bcs index
new <- read_csv("new_IPC.csv") # there is no appln_id with new IPC3
load("new_2.RData")
new_2 <- EPO_IPC[EPO_IPC$appln_id %in% new_2, c(1,2,4)]
colnames(new_2)[1] <- "Citing_appln_id"
new_2 <- merge(x= new_2, y= EPO_CITATIONS[,c(4,9)], by="Citing_appln_id", all.x = TRUE)
new_2$IPC3_group <- substr(new_2$IPC,1,3)
new_2$IPC <- NULL
new_2 <- unique(new_2)
new_2 <- new_2[complete.cases(new_2$Cited_Appln_id),]
new <- EPO_IPC[EPO_IPC$appln_id %in% new$appln_id, c(1,2,4)]
colnames(new)[1] <- "Citing_appln_id"
new <- merge(x= new, y= EPO_CITATIONS[,c(4,9)], by="Citing_appln_id", all.x = TRUE)
new$IPC3_group <- substr(new$IPC,1,3)
new$IPC <- NULL
new <- unique(new)
new <- new[complete.cases(new$Cited_Appln_id),]
new <- rbind(new, new_2)
new <- unique(new)
new$prio_year_IPC3 <- interaction(new$prio_year, new$IPC3_group)

appln_id_tech_group <- read_csv("appln_id_tech_group.csv")
colnames(appln_id_tech_group)[1] <- "Citing_appln_id"
appln_id_tech_group <- merge(x= appln_id_tech_group, y= EPO_CITATIONS[,c(4,9)], 
                             by="Citing_appln_id", all.x = TRUE)
appln_id_group <- unique(appln_id_tech_group[,c(4,5)])

L <- c(split(appln_id_group$Cited_Appln_id, appln_id_group$group), 
       split(new$Cited_Appln_id, new$prio_year_IPC3))
FUN3 <- Vectorize(function(x, y) length(intersect(x, y)))
FUN4 <- Vectorize(function(x, y) length(c(x, y)))
res3 <- outer(L, L, FUN3)
rows <- which(rownames(res3) %in% unique(appln_id_group$group))
cols <- which(colnames(res3) %in% unique(new$prio_year_IPC3))
res3 <- as.vector(res3[rows, cols])
res4 <- outer(L, L, FUN4)
rows <- which(rownames(res4) %in% unique(appln_id_group$group))
cols <- which(colnames(res4) %in% unique(new$prio_year_IPC3))
res4 <- as.vector(res4[rows, cols])
lg1 <- seq(length(unique(appln_id_group$group)))
lg2 <- seq(length(unique(new$prio_year_IPC3))) + length(unique(appln_id_group$group))
nm <- do.call(rbind, strsplit(as.vector(outer(lg1, lg2, paste)), " "))
nm <- apply(nm, 1:2, function(x) names(L)[as.double(x)])
bcs_df <- setNames(cbind.data.frame(nm, res3 / res4), c("group", "compare_group", "bcs"))
bcs_df <- aggregate(bcs~., data=bcs_df, FUN=max)
bcs_df <- bcs_df[bcs_df$bcs!=0,]
bcs_df$compare_year <- substr(bcs_df$compare_group, 1, 4)
bcs_df$prio_year <- gsub(".*[_]([^.]+)[_].*", "\\1", bcs_df$group)
write.csv(bcs_df,"bcs_df.csv")


bcs_df <- read_csv("bcs_df.csv")
### take only groups of patents linked to patents with new IPC3 established in a year after
bcs_df <- bcs_df[as.numeric(bcs_df$compare_year)==as.numeric(bcs_df$prio_year)+1,]
bcs_df <- bcs_df[bcs_df$bcs>quantile(bcs_df$bcs)[2],]
# label all patents in these groups as patents with Emerging technology
appln_id_tech_group$em <- ifelse(appln_id_tech_group$group %in% bcs_df$group, 1, 0)
colnames(appln_id_tech_group)[1] <- "appln_id"
write_csv(appln_id_tech_group, 'appln_id_tech_group.csv')



#----------------------------------------------------------------------------------------------



### Logistic regression
# add output into data
merging_tech_data <- read.csv("merging_tech_data.csv")
appln_id_tech_group <- read_csv("appln_id_tech_group.csv")
merging_tech_data <- merging_tech_data[,-c(ncol(merging_tech_data),
                                           ncol(merging_tech_data)-1, 
                                           8:22)]
merging_tech_data <- merge(x=merging_tech_data, 
                           y=unique(appln_id_tech_group[,c(1, ncol(appln_id_tech_group))]),
                           by="appln_id", all.y = T)
nrow(merging_tech_data[merging_tech_data$em==0,])
merging_tech_data$appln_id <- NULL
# split data into training, cross-validation, test sets 
set.seed(123)
spec = c(train = .9, test = .05, validate = .05)
g = sample(cut(seq(nrow(merging_tech_data)), 
               nrow(merging_tech_data)*cumsum(c(0,spec)),
               labels = names(spec)))
use = split(merging_tech_data, g)
# check the size of sets
sapply(use, nrow)/nrow(merging_tech_data)
par(mfrow = c(1, 3)) 
plot(density(as.numeric(use$train$em)))
plot(density(as.numeric(use$validate$em)))
plot(density(as.numeric(use$test$em)))
merging_tech_data <- use
# train the model
model <- multinom(em ~ ., data=merging_tech_data$train, maxit=3000, MaxNWts = 84581)
# summary(model)
# Predicting the Train set results
merging_tech_data_pred = predict(model, 
                                 merging_tech_data$train[-length(merging_tech_data$train)], 
                                 decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(merging_tech_data$train$em, merging_tech_data_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_train <- sum(cm$compare)/length(cm$compare) 

precision <- posPredValue(as.factor(merging_tech_data_pred), as.factor(merging_tech_data$train$em), positive="1")
recall <- sensitivity(as.factor(merging_tech_data_pred), as.factor(merging_tech_data$train$em), positive="1")
F1_train <- (2 * precision * recall) / (precision + recall)

# Predicting the Test set results
merging_tech_data_pred = predict(model, merging_tech_data$test[-length(merging_tech_data$test)], 
                                 decision.values = TRUE, probability = TRUE)
# Making the Confusion Matrix
cm = data.frame(merging_tech_data$test$em, merging_tech_data_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_test <- sum(cm$compare)/length(cm$compare) 

precision <- posPredValue(as.factor(merging_tech_data_pred), as.factor(merging_tech_data$test$em), positive="1")
recall <- sensitivity(as.factor(merging_tech_data_pred), as.factor(merging_tech_data$test$em), positive="1")
F1_test <- (2 * precision * recall) / (precision + recall)
 
# Predicting the Cross-Validation set results
merging_tech_data_pred = predict(model, merging_tech_data$validate[-length(merging_tech_data$validate)], 
                                 decision.values = TRUE)
# Making the Confusion Matrix
cm = data.frame(merging_tech_data$validate$em, merging_tech_data_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_validate <- sum(cm$compare)/length(cm$compare) 
# F1-score
precision <- posPredValue(as.factor(merging_tech_data_pred), as.factor(merging_tech_data$validate$em), positive="1")
recall <- sensitivity(as.factor(merging_tech_data_pred), as.factor(merging_tech_data$validate$em), positive="1")
F1_validate <- (2 * precision * recall) / (precision + recall)
saveRDS(model, "model.rds")



#----------------------------------------------------------------------------------------------



### SVM 
merging_tech_data <- read.csv("merging_tech_data.csv")
appln_id_tech_group <- read_csv("appln_id_tech_group.csv")
merging_tech_data <- merging_tech_data[,-c(ncol(merging_tech_data),
                                           ncol(merging_tech_data)-1, 
                                           8:22)]
merging_tech_data <- merge(x=merging_tech_data, 
                           y=unique(appln_id_tech_group[,c(1, ncol(appln_id_tech_group))]),
                           by="appln_id", all.y = T)
merging_tech_data$appln_id <- NULL
# split data into training, cross-validation, test sets 
set.seed(123)
spec = c(train = .9, test = .05, validate = .05)
g = sample(cut(seq(nrow(merging_tech_data)), 
               nrow(merging_tech_data)*cumsum(c(0,spec)),
               labels = names(spec)))
use = split(merging_tech_data, g)
# check the size of sets
sapply(use, nrow)/nrow(merging_tech_data)
par(mfrow = c(1, 3)) 
plot(density(as.numeric(use$train$em)))
plot(density(as.numeric(use$validate$em)))
plot(density(as.numeric(use$test$em)))
merging_tech_data <- use
# train the model
model_svm <- svm(em ~ ., data=merging_tech_data$train, 
                   type= 'C-classification', kernel= 'radial', gamma=0.1, cost=10)
summary(model_svm)
# Predicting the Train set results
merging_tech_data_pred = predict(model_svm, 
                                 merging_tech_data$train[-length(merging_tech_data$train)], 
                                 decision.values = TRUE)
# Making the Confusion Matrix
cm = data.frame(merging_tech_data$train$em, merging_tech_data_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_train_svm <- sum(cm$compare)/length(cm$compare) 

precision <- posPredValue(as.factor(merging_tech_data_pred), as.factor(merging_tech_data$train$em), positive="1")
recall <- sensitivity(as.factor(merging_tech_data_pred), as.factor(merging_tech_data$train$em), positive="1")
F1_train_svm <- (2 * precision * recall) / (precision + recall)

# Predicting the Test set results
merging_tech_data_pred = predict(model_svm, merging_tech_data$test[-length(merging_tech_data$test)], 
                                 decision.values = TRUE)
# Making the Confusion Matrix
cm = data.frame(merging_tech_data$test$em, merging_tech_data_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_test_svm <- sum(cm$compare)/length(cm$compare) 
# F1-score
precision <- posPredValue(as.factor(merging_tech_data_pred), as.factor(merging_tech_data$test$em), positive="1")
recall <- sensitivity(as.factor(merging_tech_data_pred), as.factor(merging_tech_data$test$em), positive="1")
F1_test_svm <- (2 * precision * recall) / (precision + recall)


# Predicting the Cross-Validation set results
merging_tech_data_pred = predict(model_svm, merging_tech_data$validate[-length(merging_tech_data$validate)], 
                                 decision.values = TRUE)
# Making the Confusion Matrix
cm = data.frame(merging_tech_data$validate$em, merging_tech_data_pred)
names(cm) <- c('obs', 'predicted')
cm$compare <- ifelse(cm$obs==cm$predicted, 1,0)
right_percent_validate_svm <- sum(cm$compare)/length(cm$compare) 
# F1-score
precision <- posPredValue(as.factor(merging_tech_data_pred), as.factor(merging_tech_data$validate$em), positive="1")
recall <- sensitivity(as.factor(merging_tech_data_pred), as.factor(merging_tech_data$validate$em), positive="1")
F1_validate_svm <- (2 * precision * recall) / (precision + recall)
saveRDS(model_svm, "model_svm.rds")


#-------------------------------------------------------------------------------------------

# df to summarize performance of models
evaluate_df <- data.frame(Index = c("Accuracy for training set", "Accuracy for test set",
                                  "Accuracy for CV set",
                                  "F1 for training set", "F1 for test set", "F1 for CV set"),
                          Logistic = c(right_percent_train, right_percent_test, right_percent_validate,
                                      F1_train, F1_test, F1_validate),
                          SVM = c(right_percent_train_svm, right_percent_test_svm, right_percent_validate_svm,
                                  F1_train_svm, F1_test_svm, F1_validate_svm))
write.csv(evaluate_df, "evaluate_emerging.csv")
# svm overfits

library(data.table)
library(plyr)
library(ada)
library(gbm)
library(randomForest)
library(party)
library(mboost)
library(e1071)
library(caret)
library(RCurl)
library(tm)
library(kernlab)
library(ipred)

rm(list = ls()) #remove all variables

files = c('D:/SatRday/Mortality_and_causes_of_death_from_death_notification_2012.pdf')
pdftotextpath = 'C:/Program Files/xpdfbin-win-3.04/bin64/'
#if(all(file.exists(Sys.which(c("pdfinfo", "pdftotext"))))) {
  dat = readPDF(control=list(text="-layout"))(elem=list(uri=files), 
                                             language="en", id="id1")
#}
  
dat = c(as.character(dat))
dat = read.table(text=dat, sep="|", quote="", stringsAsFactors=FALSE)

b = 0
list.rows = list()

for (a in 1:nrow(dat)){
  if(!is.na(dat[a, 1]) && 
     ((nchar(dat[a, 1]) >= 5 && substr(dat[a, 1], 1, 5) == 'Notes') ||
    !is.na(as.numeric(gsub( " .*$", "", dat[a, 1]))))){
    
    b = b + 1
    list.rows[[b]] = a
    
  }
}

dat1 = as.data.frame(dat[unlist(list.rows), ])
dat1$`dat[unlist(list.rows), ]` = lapply(dat1$`dat[unlist(list.rows), ]`, as.character)

dat1$`dat[unlist(list.rows), ]` = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", dat1$`dat[unlist(list.rows), ]`, perl=TRUE)

dat1$code = gsub( " .*$", "", dat1$`dat[unlist(list.rows), ]`)
dat1$`dat[unlist(list.rows), ]` = lapply(dat1$`dat[unlist(list.rows), ]`, as.character)
#dat1$name = gsub(" .*$", "", gsub(paste(dat1$code, ' ', sep = ''), '', dat1$`dat[unlist(list.rows), ]`))
 for (a in 1:nrow(dat1)){
   if (length(strsplit(as.character(dat1[a, 'dat[unlist(list.rows), ]']),' ')[[1]]) > 1){
#     dat1$name = strsplit(as.character(dat1[a, 'dat[unlist(list.rows), ]']),' ',fixed=TRUE)[[1]][2]
     if (a >= 460 && a <= 472){
       dat1[a, 'name'] = paste(strsplit(as.character(dat1[a, "dat[unlist(list.rows), ]"]), ' ')[[1]][2], 
                               strsplit(as.character(dat1[a, "dat[unlist(list.rows), ]"]), ' ')[[1]][3], sep = ' ')
     }else{
      dat1[a, 'name'] = strsplit(as.character(dat1[a, "dat[unlist(list.rows), ]"]), ' ')[[1]][2]  
     #dat1[a, 'name'] = gsub("(?<=[\\s]).*$", "", gsub(paste(dat1[a, 'code'], ' ', sep = ''), '', dat1[a, 'dat[unlist(list.rows), ]']))
     }
   }else{
     dat1$name = strsplit(as.character(dat1[a, 'dat[unlist(list.rows), ]']),' ')[[1]][1]
   }
 }

#dat1$name = gsub(" *\\ (.*)\\ .*", "\\1", dat1$`dat[unlist(list.rows), ]`)

mortality.data = read.table("D:/SatRday/Causes_Of_Death_-_2012.csv", header=TRUE, sep=",", stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"), quote="\"")

filtered.mortality.data = mortality.data[mortality.data$DeathType == 1 & 
                                           mortality.data$Underlying_Broad_Grp != 'R95-R99' & 
                                           mortality.data$EduCode < 97 & 
                                           mortality.data$OccupationGrp != 99 & 
                                           mortality.data$Smoker <= 2 & 
                                           mortality.data$ResProv != 97 & 
                                           mortality.data$Sex != 9 & 
                                           -which(mortality.data$MStatus %in% c(8, 9)) & 
                                           mortality.data$AgeYear != 999, 
                                         which(names(mortality.data) %in% c('AgeYear', 'Sex', 'MStatus', 'EduCode', 
                                                                            'OccupationGrp', 'Smoker', 'ResProv', 'Underlying_Broad_Grp'))]

filtered.mortality.data$Underlying_Broad_Grp = gsub('\\-', '\\.\\.\\.', filtered.mortality.data$Underlying_Broad_Grp)

#filtered.mortality.data[-which(names(filtered.mortality.data) %in% c('AgeYear'))] = lapply(filtered.mortality.data[-which(names(filtered.mortality.data) %in% c('AgeYear'))], as.numeric)

training.df = filtered.mortality.data

training.df = training.df[complete.cases(training.df), ]

list.training.df.df = list()
list.training.df.df.2 = list()
list.training.df.df.3 = list()
list.factors = list()
training.df2 = data.frame()
add.unique.rows = list()
b = 0

if(nrow(training.df) > 2500){
  for (a in 1:ncol(training.df)){
    if(colnames(training.df)[a] != 'AgeYear'){
    #if (is.factor(training.df[ , a])){
      b = b + 1
      list.factors[[b]] = a
      list.training.df.df[[b]] = training.df[!duplicated(training.df[ , a]), ]
      

      if (nrow(training.df2) == 0){
        training.df2 = list.training.df.df[[b]]
      }else{
        #for (d in 1:nrow(list.training.df.df[[b]])){
          add.unique = list.training.df.df[[b]][ , a][!list.training.df.df[[b]][ , a] %in% training.df2[ , a]]
          #add.unique = as.factor(add.unique)
          #add.unique.rows = unique(list(unlist(add.unique.rows), match(add.unique, training.df[ , a])))
        #}
        training.df2 = rbind(training.df2, list.training.df.df[[b]][list.training.df.df[[b]][ , a] %in% unlist(add.unique), ])
        #training.df2 = do.call(rbind, list.training.df.df)
        #training.df2 = training.df2[!duplicated(training.df2[ , unlist(list.factors)]), ]
      }
      
      training.df3 = training.df[!row.names(training.df) %in% row.names(training.df2), ]
      list.training.df.df.2[[b]] = training.df3[!duplicated(training.df3[ , a]), ]
      training.df2 = rbind(training.df2, list.training.df.df.2[[b]])
      
    }
  }

  training.df3 = training.df[!row.names(training.df) %in% row.names(training.df2), ]

  if (nrow(training.df2) < 1000){
    training.df = rbind(training.df2, training.df3[sample(nrow(training.df3), 1000 - nrow(training.df2)), ])
  }else{
    training.df = training.df2
  }
}

training.df = training.df[complete.cases(training.df), ]

training.df = training.df[with(training.df, ave(Underlying_Broad_Grp, Underlying_Broad_Grp,FUN=length)) > 1, ]

for (a in 1:nrow(training.df)){
  training.df[a, 'Sex'] = lapply(list, function(x) dat1[433:435, ][dat1[433:435, 'code'] == training.df[a, 'Sex'], 'name'][!is.na(dat1[433:435, ][dat1[433:435, 'code'] == training.df[a, 'Sex'], 'name'])])[[1]]
  training.df[a, 'MStatus'] = lapply(list, function(x) dat1[437:443, ][dat1[437:443, 'code'] == training.df[a, 'MStatus'], 'name'][!is.na(dat1[437:443, ][dat1[437:443, 'code'] == training.df[a, 'MStatus'], 'name'])])[[1]]
  training.df[a, 'EduCode'] = lapply(list, function(x) dat1[459:476, ][dat1[459:476, 'code'] == training.df[a, 'EduCode'], 'name'][!is.na(dat1[459:476, ][dat1[459:476, 'code'] == training.df[a, 'EduCode'], 'name'])])[[1]]
  training.df[a, 'OccupationGrp'] = lapply(list, function(x) dat1[523:533, ][dat1[523:533, 'code'] == training.df[a, 'OccupationGrp'], 'name'][!is.na(dat1[523:533, ][dat1[523:533, 'code'] == training.df[a, 'OccupationGrp'], 'name'])])[[1]]
  training.df[a, 'Smoker'] = lapply(list, function(x) dat1[682:687, ][dat1[682:687, 'code'] == training.df[a, 'Smoker'], 'name'][!is.na(dat1[682:687, ][dat1[682:687, 'code'] == training.df[a, 'Smoker'], 'name'])])[[1]]
  training.df[a, 'ResProv'] = lapply(list, function(x) dat1[739:750, ][dat1[739:750, 'code'] == training.df[a, 'ResProv'], 'name'][!is.na(dat1[739:750, ][dat1[739:750, 'code'] == training.df[a, 'ResProv'], 'name'])])[[1]]
}

for (a in 1:ncol(training.df)){
  training.df[ , a] = gsub('\\ ', '\\.\\.\\.', training.df[ , a])
}

training.df[-which(names(training.df) %in% c('AgeYear'))] = lapply(training.df[-which(names(training.df) %in% c('AgeYear'))], as.factor)
#training.df[which(names(training.df) %in% c('AgeYear'))] = lapply(training.df[which(names(training.df) %in% c('AgeYear'))], as.numeric)
training.df['AgeYear'] = lapply(training.df['AgeYear'], function(x) paste('X', x, sep = ''))
training.df[which(names(training.df) %in% c('AgeYear'))] = lapply(training.df[which(names(training.df) %in% c('AgeYear'))], as.factor)

#training.df = training.df[c('Underlying_Broad_Grp',  colnames(training.df[ , -which(names(training.df) %in% c('Underlying_Broad_Grp'))]))]
training.df = training.df[c('AgeYear',  colnames(training.df[ , -which(names(training.df) %in% c('AgeYear'))]))]

colnames(training.df)[which(names(training.df) == "Underlying_Broad_Grp")] <- "UnderlyingBroadGrp"
colnames(dat1)[1] <- "Col1"

dat[ , 1] = make.names(dat[ , 1])
dat1[ , 1] = make.names(dat1[ , 1])
training.df[ , "OccupationGrp"] = make.names(training.df$OccupationGrp)
training.df$ResProv = make.names(training.df$ResProv)

set.seed(9)

# if (nrow(training.df) < 2500){
#control = trainControl(method = 'repeatedcv', number = 10, repeats = 10)
# }else{
control = trainControl(method = 'cv', number = 3, classProbs = TRUE)
# }
#names(control) = make.names(names(control))
#control = trainControl(method = 'repeatedcv', number = 10, repeats = 10)
#fit.tree = train(training[ , 2:ncol(training)], training[ , 1], method = 'blackboost', metric = 'Accuracy', trControl=control)
#fit.tree = train(Narratives ~ ., data = training, method = 'blackboost', metric = 'Accuracy', trControl=control)

#finalModel = train(Underlying_Broad_Grp ~ ., data = training.df, method = 'gbm', metric = 'Accuracy', trControl=control)
#finalModel = train(Underlying_Broad_Grp ~ ., data = training.df, method = 'svmRadial', trControl=control)

finalModel = train(AgeYear ~ ., data = training.df, method = 'treebag', trControl=control)
#finalModel = train(Underlying_Broad_Grp ~ ., data = training.df, method = 'treebag', trControl=control)

#fit.tree = train(Narratives ~ ., data = training, method = 'rf', metric = 'Accuracy', trControl = control, ntree = 25000, na.action = na.omit)

print(finalModel)
#print(fit.tree$finalModel)

set.seed(9)
#finalModel = gbm()
#finalModel = randomForest(Narratives ~ ., training, mtry = 2, ntree = 5000)
saveRDS(finalModel, paste('C:/Users/jharmse/Documents/R/SatRday/PredictiveML/', 'finalModel', '.RDS', sep = ''))

save(list = ls(all.names = TRUE), file = "C:/Users/jharmse/Documents/R/SatRday/PredictiveML/.RData", envir = .GlobalEnv)


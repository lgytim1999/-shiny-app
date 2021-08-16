## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----packages, message=FALSE-------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(plotly)
library(Xmisc)
library(uwot)
library(msigdbr)
library(pheatmap)
library(grid)
library(gridExtra)
library(DT)
library(DMwR2)
library(viridis) 
library(limma)
#library("org.Hs.eg.db")
library(corrplot)
library(HiDimDA)
library(sparsediscrim)
library(e1071)
library(caret)
library(impute)
library(VIM)
library(janitor)
library(qtlcharts)
library(randomForest)
# library(mlr)
# library(FSelector)

#proetomics_input,metabolites_input, clinical_input

classifier=function(proetomics_input,metabolites_input,clinical_input){

## ----clinical_data_wrangling-------------------------------------------------------------------------------------------------------------

#table S1.1 covid patients clinical data

patient_clinical <- read.csv("data/patient_clinical_data.csv",
                       stringsAsFactors = FALSE,
                       check.names =  FALSE)

patient_clinical <- dplyr::rename(patient_clinical, c("Systolic_Blood_Pressure"="Systolic_BP" , "Diastolic_Blood_Pressure"="Diastolic_BP" ))
names(patient_clinical)[names(patient_clinical) == "Blood draw time point"] <- "blood_draw"
names(patient_clinical)[names(patient_clinical) == "Study Subject ID"] <- "sample_id"
names(patient_clinical)[names(patient_clinical) == "Sample ID"] <- "sample_id2"
names(patient_clinical)[names(patient_clinical) == "Who Ordinal Scale"] <- "severity_who"

# Converting sample_id2 to the same format as sample_id in patient_protein
patient_clinical <- patient_clinical %>% 
                      dplyr::select(-blood_draw, -sample_id)
patient_clinical$sample_id2 <- gsub("-1", "-BL", patient_clinical$sample_id2)
patient_clinical$sample_id2 <- gsub("-2", "-AC", patient_clinical$sample_id2)


# change the WHO ordinal scale to "Mild" and "Severe"
# patient_clinical$severity_who = as.numeric(substr(patient_clinical$severity_who, 0, 1))
# patient_clinical <- filter(patient_clinical, !is.na(severity_who))
patient_clinical <- patient_clinical %>% mutate(
  severity_who = case_when(
    severity_who == "1 or 2" ~ "1",
    TRUE ~ as.character(severity_who)
  )
) %>% filter(complete.cases(severity_who))
patient_clinical$severity_who[which(patient_clinical$severity_who <= 4)] = "Mild"
patient_clinical$severity_who[which(patient_clinical$severity_who != "Mild")] = "Severe"
dim(patient_clinical)
patient_clinical <- na.omit(patient_clinical)


## ----plasma_protein_data_wrangling-------------------------------------------------------------------------------------------------------
#table S1.3 plasma proteomics data 
plasma_proteomics <- read.csv("data/plasma_protein.csv",
                       stringsAsFactors = FALSE,
                       check.names =  FALSE)

plasma_proteomics <- plasma_proteomics[, !duplicated(colnames(plasma_proteomics))]
names(plasma_proteomics)[names(plasma_proteomics) == "Healthy donor sample or COVID19 sample"] <- "ill_status"

healthy_protein <- plasma_proteomics %>% filter(ill_status == "Healthy Donor ")
patient_protein <- plasma_proteomics %>% 
                      filter(ill_status == "COVID19 ") %>% 
                      dplyr::select(-ill_status)

plasma_proteomics <- patient_protein %>% janitor::clean_names(., "snake")
# change to omics data format
n <- plasma_proteomics$sample_id # remember name

# transpose matrix
patient_protein <- plasma_proteomics %>% dplyr::select(-sample_id)
pdata <- filter(as.data.frame(t(patient_protein)))

# get row and colnames in order
colnames(pdata) <- n
rownames(pdata) <- colnames(patient_protein)

pdata = pdata[rowSums(is.na(pdata)) < 40,]
pdata


## ----------------------------------------------------------------------------------------------------------------------------------------
# imputation of pdata using limit of detection (LoD)
for (i in 1:nrow(pdata)) {
  for (j in 1:ncol(pdata)) {
    if (is.na(pdata[i,j])) {
      lob <- mean(pdata[,j], na.rm=TRUE) + 1.645 * sd(pdata[,j], na.rm=TRUE)
      lod <- lob + 1.645 * sd(pdata[i,], na.rm=TRUE)
      pdata[i,j] <- lod
    }
  }
}



## ----------------------------------------------------------------------------------------------------------------------------------------
#table S1.4 plasma metabolites data 
plasma_metabolites <- read.csv("data/plasma_metabolites.csv",
                       stringsAsFactors = FALSE,
                       check.names =  FALSE)

patient_metabolites <- plasma_metabolites %>% filter(plasma_metabolites$`Healthy donor sample or COVID19 sample` == 'COVID19 ')
patient_metabolites


## ----------------------------------------------------------------------------------------------------------------------------------------
#clean metabolites data
meta1 <- patient_metabolites %>% janitor::clean_names() %>% as.data.frame()
colnames(meta1)[colnames(meta1)=="healthy_donor_sample_or_covid19_sample"] = "type"
dim(meta1)

mv_col <- colSums(is.na(meta1)) <= 25
meta2 <- meta1[,mv_col]
# dim(meta2)


## ----------------------------------------------------------------------------------------------------------------------------------------
sum(complete.cases(meta2) == TRUE)
for(i in 1:ncol(meta2)){
  meta2[,i][is.na(meta2[,i])] <- min(meta2[,i],na.rm = TRUE)
}
meta_full <- merge(x = patient_clinical, y = meta2, by.x = "sample_id2", by.y = "sample_id", all.y = TRUE)
# meta2


## ----------------------------------------------------------------------------------------------------------------------------------------
# transformation & normalisation
boxplot(pdata, las = 2, main = "Boxplot" ) # log transformation seems already performed, we thus omit this step.

# normalisation
pdata <- normalizeBetweenArrays(pdata, method = "scale")
boxplot(pdata, las = 2)


## ----------------------------------------------------------------------------------------------------------------------------------------
#patients in common (3 datasets)
rownames(meta_full) <- meta_full$sample_id2
# dim(pdata)
# dim(meta_full)
id <- intersect(rownames(meta_full),colnames(pdata))
final_id <- intersect(id,patient_clinical$sample_id2)
meta_full <- meta_full[final_id,]
pdata <- pdata[,final_id]
# dim(meta_full)
# dim(pdata)



## ----------------------------------------------------------------------------------------------------------------------------------------
# find intersecting patients
pclinical_id = intersect(patient_clinical$sample_id2, colnames(pdata))
plabel = patient_clinical[patient_clinical$sample_id2 %in% pclinical_id,]$severity_who



## ----accuracy_test_protein---------------------------------------------------------------------------------------------------------------
# Classifier



## ----prevalid_protein--------------------------------------------------------------------------------------------------------------------
cvk <- 5

X <- t(pdata[,])
y <- plabel

cvSets <- cvTools::cvFolds(nrow(X), cvk)

protein_knn <- protein_svm <- protein_dlda <- c()

for (j in 1:cvk) {
  test_id <- cvSets$subsets[cvSets$which == j]
  X_test <- X[test_id,]
  X_train<- X[-test_id,]
  y_test <- y[test_id]
  y_train <- y[-test_id]
  # feature selection
  design <- model.matrix(~ factor(plabel))
  
  # fit the limma model
  fit <- lmFit(pdata[,], design)
  fit2 <- eBayes(fit)
  
  tT <- topTable(fit2, coef = 2, number = Inf, sort.by ="logFC")
  
  # Select subset of proteins for training
  protein100 <- c(rownames(tT)[1:100])
  X_train <- X_train[, protein100]
 # proetomics_input<-proetomics_input[, protein100]
  
  proetomics_input<-proetomics_input[, protein100]
  
  X_test <- X_test[, protein100]
  # KNN classifier
 
   ##SVM 
  trained_svm <- svm(X_train, factor(y_train), type = "C")
 # X_realTest<-t(X_test[1:1,])
  

  predicted_svm <- predict(trained_svm, proetomics_input)
  protein_svm <- c(protein_svm, as.character(predicted_svm))

  

  
}
protein_svm <- protein_svm[seq(1, length(protein_svm), 5)]
## ----------------------------------------------------------------------------------------------------------------------------------------
mlabel1 <- meta_full$severity_who
rownames(meta2) <- meta2$sample_id
meta2 <- meta2[final_id,]
meta3 <- meta2 %>% dplyr::select(-sample_id, -type) %>% as.data.frame()
meta_norm <- normalizeBetweenArrays(meta3, method = "scale")
meta4 <- data.frame(mlabel1,meta_norm)
meta5 <- meta4 %>% dplyr::select(-mlabel1) %>% as.matrix()
mlabel <- meta4$mlabel1
# boxplot(meta_norm, las = 2, main = "Boxplot after norm" )


## ----accuracy_test_metabolite------------------------------------------------------------------------------------------------------------
# Classifier for metabolites 




## ----prevalid_metabolite, echo=FALSE,message=FALSE---------------------------------------------------------------------------------------
#feasure selection using eBayes
#dont know if this is right
X <- meta5[,]
y = mlabel

cvSets <- cvTools::cvFolds(nrow(X),nrow(X))

prevalidated_vector_meta_svm <- prevalidated_vector_meta_knn <- prevalidated_vector_meta_dlda <- NA

for (j in 1:nrow(metabolites_input)) {

    test_id <- cvSets$subsets[cvSets$which == j]
    X_test <- data.frame(X[test_id,])
    metabolites_input<-select(metabolites_input, -1)
   
    X_train<- X[-test_id,]
    y_test <- y[test_id]
    y_train <- y[-test_id]
    # feature selection
    design <- model.matrix(~ factor(mlabel))
    
    # fit the limma model
    fit <- lmFit(t(meta5[,]), design)
    fit2 <- eBayes(fit)
    tT <- topTable(fit2, coef = 2, number = Inf, sort.by ="logFC")
    
    meta100 <- c(rownames(tT)[1:100])
    
    X_train <- X_train[, meta100]
    X_test <- t(as.matrix(X_test[ meta100,]))
    
   
   
    
    ##dlda
    trained_dlda <- dlda(X_train, y_train)
    predicted_dlda <- predict(trained_dlda, metabolites_input)$class
    prevalidated_vector_meta_dlda[j] <- as.character(predicted_dlda)
}


## ----------------------------------------------------------------------------------------------------------------------------------------
####---------THESE ARE THE VARIABLES YOU MIGHT NEED FOR SHINY-APP-------------########
# proteomics data prevalidation results stored in below variables:
# protein_knn
# protein_svm
# protein_dlda

# proteomics feature selection protein selected here:
# protein100

# metabolites data prevalidation results stored in below variables:
# prevalidated_vector_meta_svm
# prevalidated_vector_meta_knn
# prevalidated_vector_meta_dlda

# metabolites feature selection protein selected here:
# meta100



## ----------------------------------------------------------------------------------------------------------------------------------------
n_patient_clinical <-  patient_clinical[patient_clinical$sample_id2 %in% pclinical_id,] %>% clean_names()
patient_clinical_train= read.csv("patient_clinical_test_data.csv",
                               stringsAsFactors = TRUE,
                               check.names =  FALSE)
## ----------------------------------------------------------------------------------------------------------------------------------------
col_needed <- c("severity_who", "sex", "age", "ethnicity", "race", "bmi", "asthma", "cancer", "chronic_hypertension", "cigarette_smoking", "chronic_kidney_disease", "congestive_heart_failure", "copd", "coronary_artery_disease", "diabetes", "temperature", "systolic_blood_pressure", "diastolic_blood_pressure")
col_needed2 <- c( "sex", "age", "bmi", "asthma", "cancer", "chronic_hypertension", "cigarette_smoking", "chronic_kidney_disease", "congestive_heart_failure", "copd", "coronary_artery_disease", "temperature", "systolic_blood_pressure", "diastolic_blood_pressure")


## ----------------------------------------------------------------------------------------------------------------------------------------
clinical_input <- as.data.frame(unclass(clinical_input))

n_patient_clinical <- n_patient_clinical[, col_needed2]
clinical_input <- clinical_input[, col_needed2]
new_patient_clinical <- cbind(clinical_input, protein_svm)
new_patient_clinical <- cbind(new_patient_clinical, prevalidated_vector_meta_dlda)



## ----------------------------------------------------------------------------------------------------------------------------------------
new_patient_clinical <- as.data.frame(unclass(new_patient_clinical), stringsAsFactors = TRUE)
new_patient_clinical$systolic_blood_pressure <- as.numeric(new_patient_clinical$systolic_blood_pressure)
new_patient_clinical$temperature <- as.numeric(new_patient_clinical$temperature)
new_patient_clinical$diastolic_blood_pressure <- as.numeric(new_patient_clinical$diastolic_blood_pressure)
new_patient_clinical$bmi <- as.numeric(new_patient_clinical$bmi)
str(new_patient_clinical)
new_patient_clinical <- new_patient_clinical %>%
  # Creating an empty column:
  add_column(severity_who = NA, .before="sex")
new_patient_clinical$severity_who=as.factor(new_patient_clinical$severity_who) 
new_patient_clinical <- as.data.frame(unclass(new_patient_clinical))

## ----logistic regression-----------------------------------------------------------------------------------------------------------------

## ----accuracy_logistic-------------------------------------------------------------------------------------------------------------------



## ----randomForest------------------------------------------------------------------------------------------------------------------------

patient_clinical_train <- patient_clinical_train[, -1]

classifier_rf <- randomForest(severity_who ~ ., data = patient_clinical_train)
summary(classifier_rf)


## ----accuracy_randomForest---------------------------------------------------------------------------------------------------------------

cv_acc_5 = c()
cv_acc_50 = c()


set.seed(1234)
n = nrow(new_patient_clinical)
K = nrow(new_patient_clinical)
X = patient_clinical_train


for (i in 1:10) {
    cvSets = cvTools::cvFolds(n, K)
    for (j in 1:K) {
        test_id <- cvSets$subsets[cvSets$which == j]
        X_test <- X[test_id, ]
        X_train <- X[-test_id, ]
        
        
      #  new_patient_clinical <- rbind(X_train[1, ] , new_patient_clinical)
       # new_patient_clinical <- new_patient_clinical[-1,]
        
       
        X_train$systolic_blood_pressure <- as.numeric(X_train$systolic_blood_pressure)
        X_train$temperature <- as.numeric(X_train$temperature)
        X_train$diastolic_blood_pressure <- as.numeric(X_train$diastolic_blood_pressure)
        X_train$bmi <- as.numeric(X_train$bmi)
        
        new_patient_clinical <- rbind(X_train[1, ] , new_patient_clinical)
        new_patient_clinical <- new_patient_clinical[-1,]
        
        classifier_rf <- randomForest(severity_who ~ ., data = X_train)
        rf_predicted <- predict(classifier_rf, newdata = new_patient_clinical)
     #   cv_acc_5[j] <-  mean(rf_predicted == X_test$severity_who)
    }
   # cv_acc_50 <- append(cv_acc_50, round(mean(cv_acc_5), 3))
    
    view(rf_predicted)
}
#view(rf_predicted)
#round(mean(cv_acc_50), 3)
}

## ----accuracy_svm------------------------------------------------------------------------------------------------------------------------
# cv_acc_5 = c()
# cv_acc_50 = c()
# 
# 
# set.seed(1234)
# n = nrow(new_patient_clinical)
# K = 10
# X = new_patient_clinical
# 
# for (i in 1:50) {
#     cvSets = cvTools::cvFolds(n, K)
#     for (j in 1:K) {
#         test_id <- cvSets$subsets[cvSets$which == j]
#         X_test <- X[test_id, ]
#         y_test <- X_test$severity_who
#         X_test <- X_test[, -c(1)]
#         X_train <- X[-test_id, ]
#         y_train <- X_train$severity_who
#         X_train <- X_train[, -c(1)]
#         classifier_svm <- svm(X_train, factor(y_train), type = "C")
#         svm_predicted <- predict(classifier_svm, newdata = X_test)
#         cv_acc_5[j] <-  mean(svm_predicted == y_test)
#     }
#     cv_acc_50 <- append(cv_acc_50, round(mean(cv_acc_5), 3))
# }
# round(mean(cv_acc_50), 3)


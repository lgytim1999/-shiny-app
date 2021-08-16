# Library
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

results = function(test_proteomics, test_metabolics, test_clinical){

# Load data
#table S1.1 covid patients clinical data
patient_clinical <- read.csv("data/patient_clinical_data.csv",
                             stringsAsFactors = FALSE,
                             check.names =  FALSE)
patient_clinical <- dplyr::rename(patient_clinical, c("Systolic_Blood_Pressure"="Systolic_BP" , "Diastolic_Blood_Pressure"="Diastolic_BP" ))
names(patient_clinical)[names(patient_clinical) == "Blood draw time point"] <- "blood_draw"
names(patient_clinical)[names(patient_clinical) == "Study Subject ID"] <- "sample_id"
names(patient_clinical)[names(patient_clinical) == "Sample ID"] <- "sample_id2"
names(patient_clinical)[names(patient_clinical) == "Who Ordinal Scale"] <- "severity_who"
patient_clinical <- patient_clinical %>% 
  dplyr::select(-blood_draw, -sample_id)
patient_clinical$sample_id2 <- gsub("-1", "-BL", patient_clinical$sample_id2)
patient_clinical$sample_id2 <- gsub("-2", "-AC", patient_clinical$sample_id2)
patient_clinical <- patient_clinical %>% mutate(
  severity_who = case_when(
    severity_who == "1 or 2" ~ "1",
    TRUE ~ as.character(severity_who)
  )
) %>% filter(complete.cases(severity_who))
patient_clinical$severity_who[which(patient_clinical$severity_who <= 4)] = "Mild"
patient_clinical$severity_who[which(patient_clinical$severity_who != "Mild")] = "Severe"
patient_clinical <- na.omit(patient_clinical)

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
n <- plasma_proteomics$sample_id # remember name
patient_protein <- plasma_proteomics %>% dplyr::select(-sample_id)
pdata <- filter(as.data.frame(t(patient_protein)))
colnames(pdata) <- n
rownames(pdata) <- colnames(patient_protein)
pdata = pdata[rowSums(is.na(pdata)) < 40,]
for (i in 1:nrow(pdata)) {
  for (j in 1:ncol(pdata)) {
    if (is.na(pdata[i,j])) {
      lob <- mean(pdata[,j], na.rm=TRUE) + 1.645 * sd(pdata[,j], na.rm=TRUE)
      lod <- lob + 1.645 * sd(pdata[i,], na.rm=TRUE)
      pdata[i,j] <- lod
    }
  }
}
pdata <- normalizeBetweenArrays(pdata, method = "scale")

#table S1.4 plasma metabolites data 
plasma_metabolites <- read.csv("data/plasma_metabolites.csv",
                               stringsAsFactors = FALSE,
                               check.names =  FALSE)
patient_metabolites <- plasma_metabolites %>% filter(plasma_metabolites$`Healthy donor sample or COVID19 sample` == 'COVID19 ')
meta1 <- patient_metabolites %>% janitor::clean_names() %>% as.data.frame()
colnames(meta1)[colnames(meta1)=="healthy_donor_sample_or_covid19_sample"] = "type"
mv_col <- colSums(is.na(meta1)) <= 25
meta2 <- meta1[,mv_col]
for(i in 1:ncol(meta2)){
  meta2[,i][is.na(meta2[,i])] <- min(meta2[,i],na.rm = TRUE)
}
meta_full <- merge(x = patient_clinical, y = meta2, by.x = "sample_id2", by.y = "sample_id", all.y = TRUE)

#patients in common (3 datasets)
rownames(meta_full) <- meta_full$sample_id2
id <- intersect(rownames(meta_full),colnames(pdata))
final_id <- intersect(id,patient_clinical$sample_id2)
meta_full <- meta_full[final_id,]
pdata <- pdata[,final_id]
pclinical_id = intersect(patient_clinical$sample_id2, colnames(pdata))
plabel = patient_clinical[patient_clinical$sample_id2 %in% pclinical_id,]$severity_who
patient_clinical <-  patient_clinical[patient_clinical$sample_id2 %in% pclinical_id,] %>% clean_names()
col_needed2 <- c("severity_who", "sex", "age", "bmi", "asthma", "cancer", "chronic_hypertension", "cigarette_smoking", "chronic_kidney_disease", "congestive_heart_failure", "copd", "coronary_artery_disease", "temperature", "systolic_blood_pressure", "diastolic_blood_pressure")
patient_clinical <- patient_clinical[, col_needed2]


# Classifier: 
# Proteomics
X_train <- t(pdata[,])
y_train_p <- plabel
protein_svm <- c()
design <- model.matrix(~ factor(plabel))
fit <- lmFit(pdata[,], design)
fit <- eBayes(fit)
tT <- topTable(fit, coef = 2, number = Inf, sort.by ="logFC")
protein100 <- c(rownames(tT)[1:100])
X_train <- X_train[, protein100]
sample_names<-test_proteomics[ , c("sample_id")]  
test_proteomics <- subset (test_proteomics, select = -sample_id)
test_proteomics <- test_proteomics[, protein100]
trained_svm <- svm(X_train, factor(y_train_p), type = "C")
predicted_svm <- predict(trained_svm, test_proteomics)
predicted_svm

# Metabolics
mlabel1 <- meta_full$severity_who
rownames(meta2) <- meta2$sample_id
meta2 <- meta2[final_id,]
meta3 <- meta2 %>% dplyr::select(-sample_id, -type) %>% as.data.frame()
meta_norm <- normalizeBetweenArrays(meta3, method = "scale")
meta4 <- data.frame(mlabel1,meta_norm)
meta5 <- meta4 %>% dplyr::select(-mlabel1) %>% as.matrix()
mlabel <- meta4$mlabel1

X_train <- meta5[,]
y_train_m = mlabel
meta_dlda <- c()
design <- model.matrix(~ factor(mlabel))
fit <- lmFit(t(meta5[,]), design)
fit <- eBayes(fit)
tT <- topTable(fit, coef = 2, number = Inf, sort.by ="logFC")
meta100 <- c(rownames(tT)[1:100])
X_train <- X_train[, meta100]
test_metabolics <- subset (test_metabolics, select = -sample_id)
test_metabolics <- test_metabolics[, meta100]
trained_dlda <- dlda(X_train, y_train_m)
predicted_dlda <- predict(trained_dlda, test_metabolics)$class
predicted_dlda

# Clinical
new_test_clinical <- cbind(test_clinical, predicted_svm)
new_test_clinical <- cbind(new_test_clinical, predicted_dlda)
new_test_clinical$systolic_blood_pressure <- as.numeric(new_test_clinical$systolic_blood_pressure)
new_test_clinical$temperature <- as.numeric(new_test_clinical$temperature)
new_test_clinical$diastolic_blood_pressure <- as.numeric(new_test_clinical$diastolic_blood_pressure)
new_test_clinical$bmi <- as.numeric(new_test_clinical$bmi)
new_test_clinical = as.data.frame(unclass(new_test_clinical), stringsAsFactors = TRUE)
new_test_clinical['results'] <- c()

new_patient_clinical <- cbind(patient_clinical, y_train_p)
new_patient_clinical <- cbind(new_patient_clinical, y_train_m)
new_patient_clinical$systolic_blood_pressure <- as.numeric(new_patient_clinical$systolic_blood_pressure)
new_patient_clinical$temperature <- as.numeric(new_patient_clinical$temperature)
new_patient_clinical$diastolic_blood_pressure <- as.numeric(new_patient_clinical$diastolic_blood_pressure)
new_patient_clinical$bmi <- as.numeric(new_patient_clinical$bmi)
new_patient_clinical <- as.data.frame(unclass(new_patient_clinical), stringsAsFactors = TRUE)
names(new_patient_clinical)[16]="predicted_svm"
names(new_patient_clinical)[17]="predicted_dlda"

levels(new_test_clinical$sex) <- levels(new_patient_clinical$sex)
levels(new_test_clinical$age) <- levels(new_patient_clinical$age)
levels(new_test_clinical$bmi) <- levels(new_patient_clinical$bmi)
levels(new_test_clinical$asthma) <- levels(new_patient_clinical$asthma)
levels(new_test_clinical$cancer) <- levels(new_patient_clinical$cancer)
levels(new_test_clinical$chronic_hypertension) <- levels(new_patient_clinical$chronic_hypertension)
levels(new_test_clinical$cigarette_smoking) <- levels(new_patient_clinical$cigarette_smoking)
levels(new_test_clinical$chronic_kidney_disease) <- levels(new_patient_clinical$chronic_kidney_disease)
levels(new_test_clinical$congestive_heart_failure) <- levels(new_patient_clinical$congestive_heart_failure)
levels(new_test_clinical$copd) <- levels(new_patient_clinical$copd)
levels(new_test_clinical$coronary_artery_disease) <- levels(new_patient_clinical$coronary_artery_disease)
levels(new_test_clinical$temperature) <- levels(new_patient_clinical$temperature)
levels(new_test_clinical$systolic_blood_pressure) <- levels(new_patient_clinical$systolic_blood_pressure)
levels(new_test_clinical$diastolic_blood_pressure) <- levels(new_patient_clinical$diastolic_blood_pressure)

set.seed(1234)
X_train = new_patient_clinical
classifier_rf <- randomForest(severity_who ~ ., data = X_train)
predicted_rf <- predict(classifier_rf, newdata = new_test_clinical)

# final results table
final_results <- data.frame(cbind(sample_names,predicted_rf))
final_results$predicted_rf <- as.character(final_results$predicted_rf)
final_results$predicted_rf[final_results$predicted_rf == "1"] <- "Mild"
final_results$predicted_rf[final_results$predicted_rf == "2"] <- "Severe"
colnames(final_results) <- c("sample_id", "Severity")
view(final_results)

return(final_results)

}

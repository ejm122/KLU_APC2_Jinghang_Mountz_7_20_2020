## @knitr CollectData
rm(list=ls())
dev.off()
pacman::p_load(rio)
library(tibble)
# IMPORTING Data ###########################################################
data <- import("~/Desktop/GitHub/Mean_Activation_AI/Appending_to_Master/KLU_APC2_Master_2020_07_01.xlsx")
Scan_Number <- import("~/Desktop/GitHub/Mean_Activation_AI/Appending_to_Master/NormalAging_DataGathering.xlsx")
activation <- import("~/Desktop/GitHub/Mean_Activation_AI/Appending_to_Master/activ_values.txt")
AI <- import("~/Desktop/GitHub/Mean_Activation_AI/Appending_to_Master/AI.txt")
FWHM <- import("~/Desktop/GitHub/Mean_Activation_AI/Appending_to_Master/FWHM.txt")
FWHM <- abs(FWHM)
# Filter Data ##############################################################
data <- data[-c(which(data$FaceNames_Exclude == 'Yes')),] #Exclude for scanning issues

list <- match(Scan_Number$ScanID,data$Vault_Scan_ID) #Only include 1 scan/subject
index <- which(list!=0,arr.ind = T)
list <- na.omit(match(Scan_Number$ScanID, data$Vault_Scan_ID))
data$Scan_Number <- NA
data$Scan_Number[list] <- Scan_Number$`Scan Number`[index]
data <- data[c(which(data$Scan_Number == 1)),]

list <- match(activation$Scan_ID,data$Vault_Scan_ID) #Match up all calculated valuese with correct scan
index <- which(list!=0,arr.ind = T)
list <- na.omit(match(activation$Scan_ID, data$Vault_Scan_ID))

#creating new variables that are going to be appended
data$Subject_ID <- NA
data$Scan_ID <- NA
data$Left_Hippocampus_Activation <- NA
data$Right_Hippocampus_Activation <- NA
data$Left_DLPFC_Activation <- NA
data$Right_DLPFC_Activation <- NA
data$Hippocampus_AI <- NA
data$DLPFC_AI <-NA
data$Left_Hippocampus_FWHM <- NA
data$Right_Hippocampus_FWHM <- NA
data$Left_DLPFC_FWHM <- NA
data$Right_DLPFC_FWHM <- NA

#appending
data$Subject_ID[list] <- activation$Subject_ID[index]
data$Scan_ID[list] <- activation$Scan_ID[index]
data$Left_Hippocampus_Activation[list] <- activation[,3][index]
data$Right_Hippocampus_Activation[list] <- activation[,4][index]
data$Left_DLPFC_Activation[list] <- activation[,5][index]
data$Right_DLPFC_Activation[list] <- activation[,6][index]
data$Hippocampus_AI[list] <- AI[,3][index]
data$DLPFC_AI[list] <- AI[,4][index]

data$Left_Hippocampus_FWHM[list] <- FWHM[,3]
data$Right_Hippocampus_FWHM[list] <- FWHM[,4]
data$Left_DLPFC_FWHM[list] <- FWHM[,5]
data$Right_DLPFC_FWHM[list] <- FWHM[,6]
# Recode Variables ##############################################################
data$Race_cat <- data$Race != 'White' #not white = 1 (True)
data$Education_cat <- data$Education > 12  #higher education = 1 (True)
data$Sex_cat <- (data$Sex == 'Male') #1 = male
data$Sex_cat[data$Sex == "NaN"] = NA
data$LETTER_FLUENCY <- (data$FLUENA + data$FLUENF+ data$FLUENS) / 3
data$STRINTERFERENCE <- (data$STRCW - data$STRCOL) / data$STRCOL
data$PiB_STATUS_CODE <- (data$PiBStatus_SUVR_GTM_FS_Global == "pos")
data$PiB_STATUS_CODE[data$PiBStatus_SUVR_GTM_FS_Global == "NaN"] = NA
data$APOE_CODE[data$APOE_CODE == "NaN"] = NA
data$Abs_Hippocampus_AI <- abs(data$Hippocampus_AI)
data$Abs_DLPFC_AI <- abs(data$DLPFC_AI)
data$FaceName_PostScanAccuracy[data$FaceName_PostScanAccuracy == "NA"] <- NA
data$FaceName_PostScanAccuracy <- as.numeric(data$FaceName_PostScanAccuracy)

#identifying PiB(+) subjects
x_l_h <- data$Left_Hippocampus_Activation[data$PiB_STATUS_CODE == TRUE]
y_l_h <- data$Left_Hippocampus_FWHM[data$PiB_STATUS_CODE == TRUE]
x_r_h <- data$Right_Hippocampus_Activation[data$PiB_STATUS_CODE == TRUE]
y_r_h <- data$Right_Hippocampus_FWHM[data$PiB_STATUS_CODE == TRUE]

x_l_d <-data$Left_DLPFC_Activation[data$PiB_STATUS_CODE == TRUE]
y_l_d <- data$Left_DLPFC_FWHM[data$PiB_STATUS_CODE == TRUE]
x_r_d <-data$Right_DLPFC_Activation[data$PiB_STATUS_CODE == TRUE]
y_r_d <- data$Right_DLPFC_FWHM[data$PiB_STATUS_CODE == TRUE]
#visulize data#################################################################
plot(data$Left_Hippocampus_Activation, data$Left_Hippocampus_FWHM,col="red", pch = 1, xlab="Mean Activation", ylab="FWHM")
points(data$Right_Hippocampus_Activation, data$Right_Hippocampus_FWHM, pch = 2, col="black")
points(x_l_h,y_l_h,pch = 4, cex = 2)
points(x_r_h,y_r_h,pch = 4, cex = 2)
legend(x=-2,y=20,c("Left Hippocampus", "Right Hippocampus", "PiB(+) Subjects"),cex=.8,col=c("red","black","black"),pch=c(1,2,4))

plot(data$Left_DLPFC_Activation, data$Left_DLPFC_FWHM,col="blue", pch = 1, xlab="Mean Activation", ylab="FWHM")
points(data$Right_DLPFC_Activation, data$Right_DLPFC_FWHM, col = "brown", pch =2)
points(x_l_d,y_l_d,pch = 4, cex = 2)
points(x_r_d,y_r_d,pch = 4, cex = 2)
legend(x=-4,y=20,c("Left DLPFC", "Right DLPFC","PiB(+) Subjects"),cex=.8,col=c("blue","brown","black"),pch=c(1,2,4))

# Cognitive Data Normalization ##################################################
#Memory
REYIM_Mean <- 15.4 #doi: 10.1136/jnnp.2004.045567
REYDE_Mean <- 14.7 #doi: 10.1136/jnnp.2004.045567
#Visiospatial
REYCO_Mean <-22.3 #doi: 10.1136/jnnp.2004.045567
BLOCKDES_Mean <- 11.4 #doi: 10.1136/jnnp.2004.045567
#Language
BOSTON1_Mean <- 26.9 #doi: 10.1136/jnnp.2004.045567
FLUEN_Mean <- 15.6 #doi: 10.1136/jnnp.2004.045567
#Executive/Attention
TRAILAS_Mean <- 45.6 #doi: 10.1136/jnnp.2004.045567
TRAILBS_Mean <-107.5 #doi: 10.1136/jnnp.2004.045567
SPANSF_Mean <- 6.4 #doi: 10.1136/jnnp.2004.045567
SPANSB_Mean <- 4.4 #doi: 10.1136/jnnp.2004.045567
DIGSYMWR_Mean <- 46.8 #doi: 10.1136/jnnp.2004.045567

#Citation Notes: - add n values
#Our sample: avg age: 74.8; >12 yrs education: 72%, avg. education: 14.897

#doi: 10.1136/jnnp.2004.045567 - avg. age (for normal subjects): 79.5; >12 yrs education: 61.5%
#Most means from this source are more low/poor compared to our sample

# Z Transform ####################################################################
#Negative z value means that lower value = higher performance
# doi:10.1016/j.jalz.2017.12.003 - method of composite calculation
#doi/ 10.1136/jnnp.2004.045567 - standard deviations from normative data
REYIM_Z <- (data$REYIM-REYIM_Mean) / 4.8
REYDE_Z <- (data$REYDE-REYDE_Mean) / 4.8
REYCO_Z <- (data$REYCO-REYCO_Mean) / 2.1
BLOCKDES_Z <- (data$BLOCKDES-BLOCKDES_Mean) / 4.8
BOSTON1_Z <-(data$BOSTON1-BOSTON1_Mean) / 2.6
FLUEN_Z <- (data$FLUEN-FLUEN_Mean) / 4.8
TRAILAS_Z <- (data$TRAILAS-TRAILAS_Mean) / 17.5
TRAILAS_Z_INV <- -1*TRAILAS_Z
SPANSF_Z <- (data$SPANSF-SPANSF_Mean) / 1.2
SPANSB_Z <- (data$SPANSB-SPANSB_Mean) / 1.2
TRAILBS_Z <- (data$TRAILBS-TRAILBS_Mean) / 49.3
TRAILBS_Z_INV <- -1*TRAILBS_Z
DIGSYMWR_Z <- (data$DIGSYMWR-DIGSYMWR_Mean) / 12.3

# Domain Scores #########################################################################
#doi:10.1016/j.jalz.2017.12.003., doi:10.1080/13607860903071014. (Both Beth Snitz articles), https://www.ncbi.nlm.nih.gov/books/NBK285344/ - for SPANSB in Executive
data$memory <- (REYIM_Z + REYDE_Z) /2
data$visiospatial <- (REYCO_Z + BLOCKDES_Z)/2
data$language <- (BOSTON1_Z +FLUEN_Z) / 2
data$executive <- (TRAILBS_Z_INV +SPANSB_Z) / 2
data$attention <- (TRAILAS_Z_INV + SPANSF_Z) / 2
data$executive_attention <- (TRAILAS_Z_INV + TRAILBS_Z_INV + SPANSF_Z + SPANSB_Z + DIGSYMWR_Z) / 5

#Intraclass Correlation ################################################################
# Pearson (Linear Correlation between composite and raw scores)
library("irr")
#Memory
REYIM_Pearson_Correlation <- cor(data$memory, data$REYIM, use = "complete.obs")
REYDE_Pearson_Correlation <- cor(data$memory, data$REYDE, use = "complete.obs")

# Visiospatial
BLOCKDES_Pearson_Correlation <- cor(data$visiospatial, data$BLOCKDES, use = "complete.obs")
REYCO_Pearson_Correlation <- cor(data$visiospatial, data$REYCO, use = "complete.obs")

#Langugae
BOSTON1_Pearson_Correlation <- cor(data$language, data$BOSTON1, use = "complete.obs")
FLUEN_Pearson_Correlation <- cor(data$language, data$FLUEN, use = "complete.obs")

#Executive
TRAILBS_Pearson_Correlation <- cor(data$executive, -1*data$TRAILBS, use = "complete.obs")
SPANSB_Pearson_Correlation <- cor(data$executive, data$SPANSB, use = "complete.obs")

#Attention
TRAILAS_Pearson_Correlation <- cor(data$attention, -1*data$TRAILAS, use = "complete.obs")
SPANSF_Pearson_Correlation <- cor(data$attention, data$SPANSF, use = "complete.obs")

#Executive_Attention
TRAILBS_Combo_Pearson_Correlation <- cor(data$executive_attention, -1*data$TRAILBS, use = "complete.obs")
TRAILAS_Combo_Pearson_Correlation <- cor(data$executive_attention, -1*data$TRAILAS, use = "complete.obs")
SPANSF_Combo_Pearson_Correlation <- cor(data$executive_attention, data$SPANSF, use = "complete.obs")
DIGSYMWR_Combo_Pearson_Correlation <- cor(data$executive_attention, data$DIGSYMWR, use = "complete.obs")
SPANSB_Combo_Pearson_Correlation <- cor(data$executive_attention, data$SPANSB, use = "complete.obs")

#Interclass Correlation (correlation between z-scores within composite score) ########3
#https://www.datanovia.com/en/lessons/intraclass-correlation-coefficient-in-r/
memory_icc_scores <-  cbind(REYIM_Z, REYDE_Z)
memory_icc_values <- icc(memory_icc_scores, model = "twoway", type = "agreement", unit = "single")
memory_icc <- memory_icc_values$value

visiospatial_icc_scores <- cbind(REYCO_Z,BLOCKDES_Z)
visiospatial_icc_values <- icc(visiospatial_icc_scores, model = "twoway", type = "agreement", unit = "single")
visiospatial_icc <- visiospatial_icc_values$value

language_icc_scores <- cbind(BOSTON1_Z,FLUEN_Z)
language_icc_values <- icc(language_icc_scores, model = "twoway", type = "consistency", unit = "single")
language_icc <- language_icc_values$value

executive_icc_scores <- cbind(TRAILBS_Z_INV,SPANSB_Z)
executive_icc_values <- icc(executive_icc_scores, model = "twoway", type = "consistency", unit = "single")
executive_icc <- executive_icc_values$value

attention_icc_scores <- cbind(TRAILAS_Z_INV,SPANSF_Z)
attention_icc_values <- icc(attention_icc_scores, model = "twoway", type = "consistency", unit = "single")
attention_icc <- attention_icc_values$value

executive_attention_icc_scores <- cbind(TRAILAS_Z_INV,TRAILBS_Z_INV,SPANSF_Z,SPANSB_Z,DIGSYMWR_Z)
executive_attention_icc_values <- icc(executive_attention_icc_scores, model = "twoway", type = "consistency", unit = "single")
executive_attention_icc <- executive_attention_icc_values$value

# Association with AI ####################################################################
mdl_hippocampus_AI <- lm(Hippocampus_AI ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_hippocampus_AI)

mdl_DLPFC_AI <- lm(DLPFC_AI ~  Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_DLPFC_AI)

# Association with Absolute AI ####################################################################
mdl_Abs_hippocampus_AI <- lm(Abs_Hippocampus_AI ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_Abs_hippocampus_AI)

mdl_Abs_DLPFC_AI <- lm(Abs_DLPFC_AI ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_Abs_DLPFC_AI)

# Association with Activation ###################################################
mdl_hippocampus_L_Activation <- lm(Left_Hippocampus_Activation ~ FaceName_PostScanAccuracy+Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_hippocampus_L_Activation)

mdl_hippocampus_R_Activation <- lm(Right_Hippocampus_Activation ~ FaceName_PostScanAccuracy+Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_hippocampus_R_Activation)

mdl_DLPFC_L_Activation <- lm(Left_DLPFC_Activation ~ FaceName_PostScanAccuracy+Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_DLPFC_L_Activation)

mdl_DLPFC_R_Activation <- lm(Right_DLPFC_Activation ~ FaceName_PostScanAccuracy+Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_DLPFC_R_Activation)

# Association with Task Accuracy
#Memory, Executive, Executive_attention
mdl_memory_task_accuracy <- lm(memory~FaceName_PostScanAccuracy, data = data)
summary(mdl_memory_task_accuracy)

mdl_visiospatial_task_accuracy <- lm(visiospatial~FaceName_PostScanAccuracy, data = data)
summary(mdl_visiospatial_task_accuracy)

mdl_language_task_accuracy <- lm(language~FaceName_PostScanAccuracy, data = data)
summary(mdl_language_task_accuracy)

mdl_executive_task_accuracy <- lm(executive~FaceName_PostScanAccuracy, data = data)
summary(mdl_executive_task_accuracy)

mdl_attention_task_accuracy <- lm(attention~FaceName_PostScanAccuracy, data = data)
summary(mdl_attention_task_accuracy)

mdl_executive_attention_task_accuracy <- lm(executive_attention~FaceName_PostScanAccuracy, data = data)
summary(mdl_executive_attention_task_accuracy)

# AI Association with Task Accuracy ##############################################3
mdl_task_accuracy_hippocampus_AI <- lm(FaceName_PostScanAccuracy ~ Hippocampus_AI, data = data) 
summary(mdl_task_accuracy_hippocampus_AI)

mdl_task_accuracy_DLPFC_AI <- lm(FaceName_PostScanAccuracy ~ DLPFC_AI, data = data) 
summary(mdl_task_accuracy_DLPFC_AI)

# Absolute AI Association with Task Accuracy ######################################33
#Absolute DLPFC AI and Task accuracy
mdl_task_accuracy_abs_hippocampus_AI <- lm(FaceName_PostScanAccuracy ~ Abs_Hippocampus_AI, data = data) 
summary(mdl_task_accuracy_abs_hippocampus_AI)

mdl_task_accuracy_abs_DLPFC_AI <- lm(FaceName_PostScanAccuracy ~ Abs_DLPFC_AI, data = data) 
summary(mdl_task_accuracy_abs_DLPFC_AI)

# Association with Cognitive Factors ###############################################
#memory1 (hippocampus - 0.075)
mdl_memory_hippocampus <- lm(memory ~ Hippocampus_AI, data = data)
summary(mdl_memory_hippocampus)
mdl_memory_DLPFC <- lm(memory ~ DLPFC_AI, data = data)
summary(mdl_memory_DLPFC)

mdl_visiospatial_hippocampus <- lm(visiospatial ~ Hippocampus_AI, data = data)
summary(mdl_visiospatial_hippocampus)
mdl_visiospatial_DLPFC <- lm(visiospatial ~ DLPFC_AI, data = data)
summary(mdl_visiospatial_DLPFC)

mdl_language_hippocampus <- lm(language ~ Hippocampus_AI, data = data)
summary(mdl_language_hippocampus)
mdl_language_DLPFC <- lm(language ~ DLPFC_AI, data = data)
summary(mdl_language_DLPFC)

mdl_executive_hippocampus <- lm(executive ~ Hippocampus_AI, data = data)
summary(mdl_executive_hippocampus)
mdl_executive_DLPFC <- lm(executive ~ DLPFC_AI, data = data)
summary(mdl_executive_DLPFC)

mdl_attention_hippocampus <- lm(attention ~ Hippocampus_AI, data = data)
summary(mdl_attention_hippocampus)
mdl_attention_DLPFC <- lm(attention ~ DLPFC_AI, data = data)
summary(mdl_attention_DLPFC)

mdl_executive_attention_hippocampus <- lm(executive_attention ~ Hippocampus_AI, data = data)
summary(mdl_executive_attention_hippocampus)
mdl_executive_attention_DLPFC <- lm(executive_attention ~ DLPFC_AI, data = data)
summary(mdl_executive_attention_DLPFC)

# Absolute AI Association with Cognitive Factors ###############################################
mdl_memory_Abs_hippocampus <- lm(memory ~ Abs_Hippocampus_AI, data = data)
summary(mdl_memory_Abs_hippocampus)
mdl_memory_Abs_DLPFC <- lm(memory ~ Abs_DLPFC_AI, data = data)
summary(mdl_memory_Abs_DLPFC)

mdl_visiospatial_Abs_hippocampus <- lm(visiospatial ~ Abs_Hippocampus_AI, data = data)
summary(mdl_visiospatial_Abs_hippocampus)
mdl_visiospatial_Abs_DLPFC <- lm(visiospatial ~ Abs_DLPFC_AI, data = data)
summary(mdl_visiospatial_Abs_DLPFC)

mdl_language_Abs_hippocampus <- lm(language ~ Abs_Hippocampus_AI, data = data)
summary(mdl_language_Abs_hippocampus)
mdl_language_Abs_DLPFC <- lm(language ~ Abs_DLPFC_AI, data = data)
summary(mdl_language_Abs_DLPFC)

mdl_executive_Abs_hippocampus <- lm(executive ~ Abs_Hippocampus_AI, data = data)
summary(mdl_executive_Abs_hippocampus)
mdl_executive_Abs_DLPFC <- lm(executive ~ Abs_DLPFC_AI, data = data)
summary(mdl_executive_Abs_DLPFC)

mdl_attention_Abs_hippocampus <- lm(attention ~ Abs_Hippocampus_AI, data = data)
summary(mdl_attention_Abs_hippocampus)
mdl_attention_Abs_DLPFC <- lm(attention ~ Abs_DLPFC_AI, data = data)
summary(mdl_attention_Abs_DLPFC)

mdl_executive_attention_Abs_hippocampus <- lm(executive_attention ~ Abs_Hippocampus_AI, data = data)
summary(mdl_executive_attention_Abs_hippocampus)
mdl_executive_attention_Abs_DLPFC <- lm(executive_attention ~ Abs_DLPFC_AI, data = data)
summary(mdl_executive_attention_Abs_DLPFC)

# Cognitive Factors ###################################################################################################
#Executive1 (0.01), executive/attention (0.00)
mdl_memory <- lm(memory ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_memory)

mdl_visiospatial <- lm(visiospatial ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_visiospatial)

mdl_language <- lm(language ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_language)

mdl_executive <- lm(executive ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive)

mdl_attention <- lm(attention ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_attention)

mdl_executive_attention <- lm(executive_attention ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive_attention)

# Cognitive Factors with All Variables ################################################################
#executive1 (0.0299), executive_attention (0.02)
mdl_memory_all <- lm(memory ~ FaceName_PostScanAccuracy+DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_memory_all)

mdl_visiospatial_all <- lm(visiospatial ~ FaceName_PostScanAccuracy+DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_visiospatial_all)

mdl_language_all <- lm(language ~ FaceName_PostScanAccuracy+DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_language_all)

mdl_executive_all <- lm(executive ~ FaceName_PostScanAccuracy+DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive_all)

mdl_attention_all <- lm(attention ~ FaceName_PostScanAccuracy+DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_attention_all)

mdl_executive_attention_all <- lm(executive_attention ~ FaceName_PostScanAccuracy+DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive_attention_all)

# Cognitive Factors with All Variables - Absolute AI################################################################
#Language (0.06), executive1 (0.0299), executive_attention (0.02)
mdl_memory_all_abs_AI <- lm(memory ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_memory_all_abs_AI)

mdl_immediate_memory_all_abs_AI <- lm(REYIM_Z ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_immediate_memory_all_abs_AI)

mdl_delayed_memory_all_abs_AI <- lm(REYDE_Z ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_delayed_memory_all_abs_AI)

mdl_visiospatial_all_abs_AI <- lm(visiospatial ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_visiospatial_all_abs_AI)

mdl_language_all_abs_AI <- lm(language ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_language_all_abs_AI)

mdl_executive_all_abs_AI <- lm(executive ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive_all_abs_AI)

mdl_attention_all_abs_AI <- lm(attention ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_attention_all_abs_AI)

mdl_executive_attention_all_abs_AI <- lm(executive_attention ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive_attention_all_abs_AI)

# Association with FWHM ##########################################################################################
mdl_L_Hippocampus_FWHM <- lm(Left_Hippocampus_FWHM~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_L_Hippocampus_FWHM)
mdl_R_Hippocampus_FWHM <- lm(Right_Hippocampus_FWHM~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_R_Hippocampus_FWHM)

mdl_L_DLPFC_FWHM <- lm(Left_DLPFC_FWHM~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_L_DLPFC_FWHM)
mdl_R_DLPFC_FWHM <- lm(Right_DLPFC_FWHM~  Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_R_DLPFC_FWHM)

# Cognitive Score with 7 variables plus FWHM #####################################################################
mdl_memory_all <- lm(memory ~ Left_DLPFC_FWHM + Right_DLPFC_FWHM + Left_Hippocampus_FWHM + Right_Hippocampus_FWHM + Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_memory_all)

mdl_visiospatial_all <- lm(visiospatial ~ Left_DLPFC_FWHM + Right_DLPFC_FWHM + Left_Hippocampus_FWHM + Right_Hippocampus_FWHM + Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_visiospatial_all)

mdl_language_all <- lm(language ~ Left_DLPFC_FWHM + Right_DLPFC_FWHM + Left_Hippocampus_FWHM + Right_Hippocampus_FWHM + Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_language_all)
#p-value: 0.0225
mdl_executive_all <- lm(executive ~ Left_DLPFC_FWHM + Right_DLPFC_FWHM + Left_Hippocampus_FWHM + Right_Hippocampus_FWHM + Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive_all)

mdl_attention_all <- lm(attention ~ Left_DLPFC_FWHM + Right_DLPFC_FWHM + Left_Hippocampus_FWHM + Right_Hippocampus_FWHM + Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_attention_all)
#p-value: 0.002909
mdl_executive_attention_all <- lm(executive_attention ~  Left_DLPFC_FWHM + Right_DLPFC_FWHM + Left_Hippocampus_FWHM + Right_Hippocampus_FWHM + Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive_attention_all)
#p-value: 0.03174
mdl_post_scan_accuracy <- lm(FaceName_PostScanAccuracy ~  Left_DLPFC_FWHM + Right_DLPFC_FWHM + Left_Hippocampus_FWHM + Right_Hippocampus_FWHM + Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_post_scan_accuracy)
## @knitr DemographicData
# Demographics Table ###############################################################
if (!require("kableExtra")) install.packages("kableExtra")
library(knitr)
library(kableExtra)
if (!require("scales")) install.packages("scales")
library(scales)

#Calculate Data - Some data missing
num_subjects <- nrow(data)

n_education <- length(data$Education_cat[!is.na(data$Education_cat)])
mean_education <- mean(data$Education, na.rm = TRUE)
stdv_education <- sd(data$Education, na.rm = TRUE)
education_over_12 <- sum(data$Education_cat, na.rm = TRUE)
education_under_12 <- length(data$Education_cat)-sum(data$Education_cat, na.rm = TRUE) - sum(is.na(data$Education_cat))
percent_over_12 <- education_over_12 / (length(data$Education_cat) - sum(is.na(data$Education_cat)))
percent_under_12 <- education_under_12 / (length(data$Education_cat) - sum(is.na(data$Education_cat)))

n_age <- length(data$Age_CurrentVisit[!is.na(data$Age_CurrentVisit)])
mean_age <- mean(data$Age_CurrentVisit, na.rm = TRUE)
age_stdv <- sd(data$Age_CurrentVisit, na.rm = TRUE)

n_sex <- length(data$Sex_cat[!is.na(data$Sex_cat)])
male_count <- sum(data$Sex == "Male", na.rm = TRUE)
percent_male <- male_count / (length(data$Sex) - sum(data$Sex == ""))
female_count <- sum(data$Sex == "Female", na.rm = TRUE)
percent_female <- female_count / (length(data$Sex) - sum(data$Sex == ""))

n_race <- length(data$Race_cat[!is.na(data$Race_cat)])
white_count <- sum(data$Race == "White")
percent_white <- white_count / (length(data$Race) - sum(data$Race == ""))
black_count <- sum(data$Race == "Black")
percent_black <- black_count / (length(data$Race) - sum(data$Race == ""))
asian_count <- sum(data$Race == "Asian")
percent_asian <- asian_count / (length(data$Race) - sum(data$Race == ""))

#Make Table
demographic_data <- data.frame(Variable = c("Total_Subjects","n", "Mean", "Standard Deviation", "Over 12 Years", "Under 12 Years", "n","Mean", "Standard Deviation", "n","Male Count", "Female Count", "n","White Count", "Black Count", "Asian Count"),
                               Value = c(num_subjects, n_education, mean_education, stdv_education, education_over_12, education_under_12,n_age, mean_age, age_stdv, n_sex, male_count, female_count, n_race, white_count,black_count,asian_count),
                               Percent = c("", "", "","", label_percent()(percent_over_12), label_percent()(percent_under_12),"","","","",label_percent()(percent_male), label_percent()(percent_female),"",label_percent()(percent_white),label_percent()(percent_black),label_percent()(percent_asian)))

kable(demographic_data, caption = "Demographic data for normal aging", digits = 2) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right") %>%
  pack_rows("Education", 2,6) %>%
  pack_rows("Age", 7,9) %>%
  pack_rows("Sex", 10,12) %>%
  pack_rows("Race", 13,16)
## @knitr correlationTablesMake
  
# Correlation Tables ########################################################################
if (!require("formattable")) install.packages("formattable")
library(formattable)

Pearson_Correlation_data <- data.frame(Variable_1 = c("REYDE","","BLOCKDES","","BOSTON1","","TRAILBS","","TRAILAS",""),
                               Pearson_Correlation_1 = c(REYDE_Pearson_Correlation,"", BLOCKDES_Pearson_Correlation,"", BOSTON1_Pearson_Correlation,"", TRAILBS_Pearson_Correlation,"", TRAILAS_Pearson_Correlation,""),
                               Variable_2 = c("REYIM","","REYCO","","FLUEN","","SPANSB","","SPANSF",""),
                               Pearson_Correlation_2 = c(REYIM_Pearson_Correlation,"", REYCO_Pearson_Correlation,"", FLUEN_Pearson_Correlation,"", SPANSB_Pearson_Correlation,"", SPANSF_Pearson_Correlation,""))
kable(Pearson_Correlation_data, caption = "Pearson Interclass Correlation Values for Cognitive Domains", digits = 2) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right") %>%
  pack_rows("Memory Domain",1,2) %>%
  pack_rows("Visiospatial Domain",3,4) %>%
  pack_rows("Language",5,6) %>%
  pack_rows("Executive",7,8) %>%
  pack_rows("Attention",9,10)

executive_attention_values <- data.frame("DIGSYMWR_Pearson" = DIGSYMWR_Combo_Pearson_Correlation, "TRAILAS_Pearson" = TRAILAS_Combo_Pearson_Correlation, "SPANSF_Pearson" = SPANSF_Combo_Pearson_Correlation, "TRAILBS_Pearson" = TRAILBS_Combo_Pearson_Correlation, "SPANSB_Pearson" = SPANSB_Combo_Pearson_Correlation)
formattable(executive_attention_values, align = "c", caption = "Executive and Attention domain Correlations. Pearson correlations were calcuated between raw scores and domain values.  ICC values were calculated between z-scores of tests within the same domain.", header = c("Pearson Correlations", ""))

# Regression Tables/Plots ######################################################################

#Calculations for Regression Summary
rsquared <- summary(mdl_hippocampus_AI)$r.squared
fstat <- summary(mdl_hippocampus_AI)$fstatistic[1]
df1 <- summary(mdl_hippocampus_AI)$fstatistic[2]
df2 <- summary(mdl_hippocampus_AI)$fstatistic[3]
#For p-value
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
pval <- lmp(mdl_hippocampus_AI)

if (!require("sjPlot")) install.packages("sjPlot")
library(sjPlot)

#Abs_Hippocampus_AI,Abs_DLPFC_AI  ~ FaceName_PostScanAccuracy+Age_CurrentVisit + Sex_cat + Race_cat + Education_cat + FDG_SUVR_GTM_FS_Global + PiB_STATUS_CODE + APOE_CODE
#p-value: 0.1086 - Hippocampus
#p-value: 0.1412 - DLPFC
tab_model(mdl_Abs_hippocampus_AI,mdl_Abs_DLPFC_AI, dv.labels = "Hippocampus Absolute AI",
          pred.labels = c("Intercept","Age at Visit", "Sex", "Race",
                          "Education (years)", "FDG Global", 
                          "PiB Global Status", "APOE Status"), show.rshow.ci = FALSE)


tab_model(mdl_Abs_DLPFC_AI, dv.labels = "DLPFC Absolute AI",
          pred.labels = c("Intercept","Age at Visit", "Sex", "Race",
                          "Education (years)", "FDG Global", 
                          "PiB Global Status", "APOE Status"), show.ci = FALSE)

#Left_Hippocampus_Activation ~ FaceName_PostScanAccuracy + Age_CurrentVisit + Sex_cat + Race_cat + Education_cat + FDG_SUVR_GTM_FS_Global + PiB_STATUS_CODE + APOE_CODE
#p-value: 0.8967
tab_model(mdl_hippocampus_L_Activation, dv.labels = "Left Hippocampus Activation",
          pred.labels = c("Intercept", "Task Accuracy","Age at Visit", "Sex", "Race",
                          "Education (years)", "FDG Global", 
                          "PiB Global Status", "APOE Status"), show.ci = FALSE)

#FaceName_PostScanAccuracy ~ memory, executive, executive_attention
# p-value: 0.06306 - memory
#p-value: 0.02681- executive
#p-value: p-value: 0.04423 - executive_attention
tab_model(mdl_memory_task_accuracy,mdl_executive_task_accuracy,mdl_executive_attention_task_accuracy, dv.labels = c("Memory","Executive","Executive_Attention"),
          pred.labels = c("Intercept", "Task Accuracy"), show.ci = FALSE)

#FaceName_PostScanAccuracy ~ Abs_DLPFC_AI
#p-value: 0.04785
tab_model(mdl_task_accuracy_abs_DLPFC_AI, dv.labels = "Task Accuracy Vs. Absolute DLPFC AI",
          pred.labels = c("Intercept", "Absolute DLPFC AI"), show.ci = FALSE)

#Memory ~ Hippocampus AI
#p-value: 0.07482
tab_model(mdl_memory_hippocampus, dv.labels = "Memory Vs. Hippocampus Asymmetry Index", show.fstat = TRUE, show.p = TRUE, show.ci = FALSE)
#No relationship between cognitive domains and absolute AI

#memory, REYIM_Z, REYDE_Z ~ FaceName_PostScanAccuracy + Abs_DLPFC_AI + Abs_Hippocampus_AI + Age_CurrentVisit + Sex_cat + Race_cat + Education_cat + FDG_SUVR_GTM_FS_Global + PiB_STATUS_CODE + APOE_CODE
#None sigificant
tab_model(mdl_memory_all_abs_AI, mdl_immediate_memory_all_abs_AI,mdl_delayed_memory_all_abs_AI, show.ci = FALSE, show.p = TRUE, show.fstat= TRUE, dv.labels = c("Memory Domain", "REYIM", "REYDE"),
          pred.labels = c("Intercept", "Task Accuracy","Absolute DLPFC AI", "Absolute Hippocampus AI","Age at Visit", "Sex", "Race",
                          "Education (years)", "FDG Global", 
                          "PiB Global Status", "APOE Status"))

#executive ~ FaceName_PostScanAccuracy + Abs_DLPFC_AI +Abs_Hippocampus_AI + Age_CurrentVisit + Sex_cat + Race_cat + Education_cat + FDG_SUVR_GTM_FS_Global + PiB_STATUS_CODE + APOE_CODE
#p-value: 0.06034 - language
#p-value: 0.03576 - executive
#p-value: 0.02144 - executive_attention
tab_model(mdl_language_all_abs_AI, mdl_executive_all_abs_AI,mdl_executive_attention_all_abs_AI, dv.labels = c("Language", "Executive", "Executive_Attention"),
          pred.labels = c("Intercept", "Task Accuracy","Absolute DLPFC AI", "Absolute Hippocampus AI","Age at Visit", "Sex", "Race",
                          "Education (years)", "FDG Global", 
                          "PiB Global Status", "APOE Status"), show.ci = FALSE)



#plot(data$Hippocampus_AI, data$memory, ylab = "Memory Composite Score", xlab = "Hippocampus Asymmetry Index", main = "Memory Vs. Hippocampus Asymmetry Index")
#abline(mdl_memory_hippocampus, col = "red")
#plot(mdl_memory_hippocampus, main = "Memory Vs. Hippocampus Asymmtery Index")

#executive ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE
#p-value: 0.01229
#executive_attention ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE
#p-value: 0.007688
tab_model(mdl_executive, mdl_executive_attention,  
  pred.labels = c("Intercept", "Age at Visit", "Sex", "Race",
                  "Education (years)", "FDG Global", 
                  "PiB Global Status", "APOE Status"),show.ci = FALSE, dv.labels = c("Executive", "Executive and Attention"))
plot(mdl_executive, main = "Executive")
plot(mdl_executive_attention, main = "Executive and Attention")

#executive ~ DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE
# p-value: 0.02993
#executive_attention ~ DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE
#p-value: 0.02018
tab_model(mdl_executive_all, mdl_executive_attention_all,  
          pred.labels = c("Intercept", "DLPFC Asymmetry", "Hippocampus Asymmetry","Age at Visit", "Sex", "Race",
                          "Education (years)", "FDG Global", 
                          "PiB Global Status", "APOE Status"),show.ci = FALSE, dv.labels = c("Executive", "Executive and Attention"))
plot(mdl_executive_all, main = "Executive Vs. All")
plot(mdl_executive_attention_all, main = "Executive and Attention Vs. All")


#################################################################################

save.image(file = "~/Desktop/RStudio Scripts/AI_Results_2020_06_23.rda") #path for spreadsheet - saves all variables





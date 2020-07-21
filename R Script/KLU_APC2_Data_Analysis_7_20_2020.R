rm(list=ls())
dev.off()
pacman::p_load(rio)
library(tibble)
# IMPORTING Data ###########################################################
data <- import("~/Desktop/GitHub/KLU_APC2_Jinghang_Mountz_7_20_2020/Appending_to_Master/KLU_APC2_Master_2020_07_01.xlsx")
Scan_Number <- import("~/Desktop/GitHub/KLU_APC2_Jinghang_Mountz_7_20_2020/Appending_to_Master/NormalAging_DataGathering.xlsx")
activation <- import("~/Desktop/GitHub/KLU_APC2_Jinghang_Mountz_7_20_2020/Appending_to_Master/activ_values.txt")
AI <- import("~/Desktop/GitHub/KLU_APC2_Jinghang_Mountz_7_20_2020/Appending_to_Master/AI.txt")
FWHM <- import("~/Desktop/GitHub/KLU_APC2_Jinghang_Mountz_7_20_2020/Appending_to_Master/FWHM.txt")
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

# Cognitive Factors - Raw AI ################################################################
#executive1 (0.0299), executive_attention (0.02)
mdl_memory_raw_AI <- lm(memory ~ FaceName_PostScanAccuracy+DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_memory_raw_AI)

mdl_visiospatial_raw_AI <- lm(visiospatial ~ FaceName_PostScanAccuracy+DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_visiospatial_raw_AI)

mdl_language_raw_AI <- lm(language ~ FaceName_PostScanAccuracy+DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_language_raw_AI)

mdl_executive_raw_AI <- lm(executive ~ FaceName_PostScanAccuracy+DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive_raw_AI)

mdl_attention_raw_AI <- lm(attention ~ FaceName_PostScanAccuracy+DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_attention_raw_AI)

mdl_executive_attention_raw_AI <- lm(executive_attention ~ FaceName_PostScanAccuracy+DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive_attention_raw_AI)

# Cognitive Factors with All Variables - Absolute AI################################################################
#Language (0.06), executive1 (0.0299), executive_attention (0.02)
mdl_memory_abs_AI <- lm(memory ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_memory_abs_AI)

mdl_immediate_memory_abs_AI <- lm(REYIM_Z ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_immediate_memory_abs_AI)

mdl_delayed_memory_abs_AI <- lm(REYDE_Z ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_delayed_memory_abs_AI)

mdl_visiospatial_abs_AI <- lm(visiospatial ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_visiospatial_abs_AI)

mdl_language_abs_AI <- lm(language ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_language_abs_AI)

mdl_executive_abs_AI <- lm(executive ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive_abs_AI)

mdl_attention_abs_AI <- lm(attention ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_attention_abs_AI)

mdl_executive_attention_abs_AI <- lm(executive_attention ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive_attention_abs_AI)

#################################################################################

save.image(file = "~/Desktop/RStudio Scripts/AI_Results_2020_06_23.rda") #path for spreadsheet - saves all variables





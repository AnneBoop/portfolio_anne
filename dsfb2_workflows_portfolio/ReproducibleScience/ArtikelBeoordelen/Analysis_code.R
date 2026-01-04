######### load libraries and install limma package ################
library(pdftools)
library(limma)
library(dplyr)
library(readxl)
library(rvest)
library(stringr)
library(htmltools)
library(htmlwidgets)
library(tidyverse)
library(writexl)
library(robvis)
library(meta)
library(ggplot2)

###############################################
################## Analysis ###################
###############################################

#Load data *Origineel
Mastersheet <- read_excel("C:/Users/benja/Dropbox/Research/Experiments/NeuroscienceTranslation/Translation_manuscript/Mastersheet_extraction.xlsx", sheet = 4)#It was sheet 2 formerly
Mastersheet <- as.data.frame(Mastersheet)

# *Verbetering* 
Mastersheet <- read_excel("Primary_data_including_metadata.xlsx", sheet = 4)#It was sheet 2 formerly
Mastersheet <- as.data.frame(Mastersheet)

#Categorize data
Mastersheet$N_animal_studies <- as.numeric(Mastersheet$N_animal_studies)
Mastersheet$N_clinical_studies <- as.numeric(Mastersheet$N_clinical_studies)
Mastersheet$Contigency_eligibility <- as.factor(Mastersheet$Contigency_eligibility)

Mastersheet[20:32] <- lapply(Mastersheet[20:32], gsub, pattern = 0, replacement = "High")
Mastersheet[20:32] <- lapply(Mastersheet[20:32], gsub, pattern = 1, replacement = "Low")
Mastersheet[20:32] <- lapply(Mastersheet[20:32], gsub, pattern = 2, replacement = "Unclear")
Mastersheet$count.high <- apply(Mastersheet[20:32], 1, function(x) length(which(x=="High")))
Mastersheet$count.low <- apply(Mastersheet[20:32], 1, function(x) length(which(x=="Low")))
Mastersheet$count.unclear <- apply(Mastersheet[20:32], 1, function(x) length(which(x=="Unclear")))

Mastersheet <- mutate(Mastersheet,
                  Overall_rob = case_when(
                    count.low > 8 ~ "Low",
                    count.low <= 8 & count.low >= 5 ~ "Moderate",
                    TRUE ~ "High"))     
  
Mastersheet$Weight <- NA
Mastersheet$Weight <- ((Mastersheet$N_animal_studies + Mastersheet$N_clinical_studies)/
                         (sum(Mastersheet$N_animal_studies, na.rm = T)+
                            sum(Mastersheet$N_clinical_studies, na.rm = T)))*100

Mastersheet["N_animal_studies"][is.na(Mastersheet["N_animal_studies"])] <- 0
Mastersheet["N_clinical_studies"][is.na(Mastersheet["N_clinical_studies"])] <- 0
Mastersheet["Pre_positive"][is.na(Mastersheet["Pre_positive"])] <- 0
Mastersheet["Pre_negative"][is.na(Mastersheet["Pre_negative"])] <- 0
Mastersheet["Pre_mixed"][is.na(Mastersheet["Pre_mixed"])] <- 0
Mastersheet["Pre_neutral"][is.na(Mastersheet["Pre_neutral"])] <- 0
Mastersheet["Clin_positive"][is.na(Mastersheet["Clin_positive"])] <- 0
Mastersheet["Clin_negative"][is.na(Mastersheet["Clin_negative"])] <- 0
Mastersheet["Clin_mixed"][is.na(Mastersheet["Clin_mixed"])] <- 0
Mastersheet["Clin_neutral"][is.na(Mastersheet["Clin_neutral"])] <- 0
Mastersheet["RCTs"][is.na(Mastersheet["RCTs"])] <- 0
Mastersheet["RCTs_positive"][is.na(Mastersheet["RCTs_positive"])] <- 0
Mastersheet["RCTs_negative"][is.na(Mastersheet["RCTs_negative"])] <- 0
Mastersheet["RCTs_mixed"][is.na(Mastersheet["RCTs_mixed"])] <- 0
Mastersheet["RCTs_neutral"][is.na(Mastersheet["RCTs_neutral"])] <- 0

#Number of included studies
sum(Mastersheet$N_animal_studies, na.rm = T)
sum(Mastersheet$N_clinical_studies, na.rm = T)
Disease_class <- Mastersheet[c("Disease_class1", "Disease_class2")]
Disease_class <- data.frame(col = c(Disease_class$Disease_class1, Disease_class$Disease_class2))
Disease_class <- na.omit(Disease_class)
Disease_table <- table(Disease_class$col)
Disease_table <- sort(Disease_table, decreasing = TRUE)
Disease_table
nrow(Disease_class)

###Quorum flow chart * Niet uitgevoerd
Quorum <- read_excel("C:/Users/benja/Dropbox/Research/Experiments/NeuroscienceTranslation/Translation_manuscript/Quorumchart_data.xlsx", sheet = 1)
table(Quorum$`Reason for exclusion`)


#Risk of bias plot
Rob_sheet <- Mastersheet[c("Study", "Rob_protocol", "Rob_deviations", "Rob_question",
                           "Rob_guidelines", "Rob_search", "Rob_date", "Rob_string",
                           "Rob_flowchart", "Rob_inclusion",  "Rob_extraction", "Rob_rob",
                            "Rob_coi", "Overall_rob", "Weight")] 

Rob_sheet_plot <- Rob_sheet
colnames(Rob_sheet_plot)[2] <- "Pre-specified protocol"
colnames(Rob_sheet_plot)[3] <- "Protocol deviations"
colnames(Rob_sheet_plot)[4] <- "Research question"
colnames(Rob_sheet_plot)[5] <- "In accordance with guidelines"
colnames(Rob_sheet_plot)[6] <- "Search strategy in two databases"
colnames(Rob_sheet_plot)[7] <- "Search date"
colnames(Rob_sheet_plot)[8] <- "Search string"
colnames(Rob_sheet_plot)[9] <- "Study flow chart"
colnames(Rob_sheet_plot)[10] <- "Inclusion criteria appropriate"
colnames(Rob_sheet_plot)[11] <- "Screening/extraction by two reviewers"
colnames(Rob_sheet_plot)[12] <- "Risk of bias assessment performed"
colnames(Rob_sheet_plot)[13] <- "Conflict of interest"

png("Rob_plot2.png", width = 4000, height = 2800, res = 300)
rob_summary(data = Rob_sheet_plot, 
            tool = "ROB1",
            weighted = TRUE)
dev.off()

plot_rob <- rob_summary(data = Rob_sheet_plot, 
                    tool = "ROB1",
                    weighted = TRUE)

# Adjust font size
png("Rob_plot2.png", width = 5000, height = 2800, res = 300)
plot_rob +
  theme(
    text = element_text(size = 20),  # Base font size; adjust this number as needed
    axis.text.x = element_text(size = 20),  # X-axis text size
    axis.text.y = element_text(size = 20),  # Y-axis text size
    axis.title.x = element_text(size = 20),  # X-axis title size
    axis.title.y = element_text(size = 20),  # Y-axis title size
    legend.title = element_text(size = 20),  # Legend title size
    legend.text = element_text(size = 20)  # Legend text size
  )
dev.off()

--------- Tot hier uitgevoerd

### Translation meta-analysis
Translation_meta <- subset(Mastersheet, Contigency_eligibility == 1)
Translation_meta <- Translation_meta[c("Study", "Title", "N_animal_studies", "N_clinical_studies",
                                        "Pre_positive", "Pre_negative", "Pre_mixed", "Pre_neutral",
                                       "RCTs", "RCTs_positive", "RCTs_negative", "RCTs_mixed", "RCTs_neutral",
                                       "Clin_positive", "Clin_negative", "Clin_mixed", "Clin_neutral",
                                       "Overall_rob", "Disease_group", "Disease_class1", "Disease_class2")]

Translation_meta$Study <- paste(Translation_meta$Study, Translation_meta$Disease_group, sep = " ")
Translation_meta$Pre_positive <- as.numeric(Translation_meta$Pre_positive)
Translation_meta$Pre_negative <- as.numeric(Translation_meta$Pre_negative)
Translation_meta$Pre_mixed <- as.numeric(Translation_meta$Pre_mixed)
Translation_meta$Pre_neutral <- as.numeric(Translation_meta$Pre_neutral)
Translation_meta$Clin_positive <- as.numeric(Translation_meta$Clin_positive)
Translation_meta$Clin_negative <- as.numeric(Translation_meta$Clin_negative)
Translation_meta$Clin_mixed <- as.numeric(Translation_meta$Clin_mixed)
Translation_meta$Clin_neutral <- as.numeric(Translation_meta$Clin_neutral)
Translation_meta$RCTs <- as.numeric(Translation_meta$RCTs)
Translation_meta$RCTs_positive <- as.numeric(Translation_meta$RCTs_positive)
Translation_meta$RCTs_negative <- as.numeric(Translation_meta$RCTs_negative)
Translation_meta$RCTs_mixed <- as.numeric(Translation_meta$RCTs_mixed)
Translation_meta$RCTs_neutral <- as.numeric(Translation_meta$RCTs_neutral)

Translation_meta$Pre_all <- Translation_meta$Pre_positive +
                            Translation_meta$Pre_negative +
                            Translation_meta$Pre_mixed +
                            Translation_meta$Pre_neutral
Translation_meta$Clin_all <-  Translation_meta$Clin_positive +
                              Translation_meta$Clin_negative +
                              Translation_meta$Clin_mixed +
                              Translation_meta$Clin_neutral

Translation_meta$Pre_nonpositive <- Translation_meta$Pre_negative +
                                    Translation_meta$Pre_mixed +
                                    Translation_meta$Pre_neutral
Translation_meta$Clin_nonpositive <- Translation_meta$Clin_negative +
                                    Translation_meta$Clin_mixed +
                                    Translation_meta$Clin_neutral
Translation_meta$RCTs_nonpositive <- Translation_meta$RCTs_negative +
                                     Translation_meta$RCTs_mixed +
                                     Translation_meta$RCTs_neutral

Translation_meta$Pre_percentage <-  (Translation_meta$Pre_positive)/
                                    (Translation_meta$Pre_positive+Translation_meta$Pre_nonpositive)
Translation_meta$Clin_percentage <- (Translation_meta$Clin_positive)/
                                    (Translation_meta$Clin_positive+Translation_meta$Clin_nonpositive)
Translation_meta$RCTs_percentage <- (Translation_meta$RCTs_positive)/
                                    (Translation_meta$RCTs)

Translation_meta$Ratio_translation <- (Translation_meta$Clin_percentage)/
                                      (Translation_meta$Pre_percentage)
Translation_meta$Ratio_translation_RCT <- (Translation_meta$RCTs_percentage)/
                                          (Translation_meta$Pre_percentage)

Translation_meta$Clin_positive_norct <- Translation_meta$Clin_positive - Translation_meta$RCTs_positive
Translation_meta$Clin_all_norct <- Translation_meta$Clin_all - Translation_meta$RCTs

sum(Translation_meta$Pre_all)
sum(Translation_meta$Clin_all)
sum(Translation_meta$RCTs)

sum(Translation_meta$Pre_positive)
sum(Translation_meta$Clin_positive)
sum(Translation_meta$RCTs_positive)

sum(Translation_meta$Pre_neutral)
sum(Translation_meta$Clin_neutral)
sum(Translation_meta$RCTs_neutral)

sum(Translation_meta$Pre_mixed)
sum(Translation_meta$Clin_mixed)
sum(Translation_meta$RCTs_mixed)

sum(Translation_meta$Pre_negative)
sum(Translation_meta$Clin_negative)
sum(Translation_meta$RCTs_negative)

Translation_meta <- subset(Translation_meta, Pre_all > 4)#Only including therapies with 5 or more animal studies published


##Forest plot, meta-analysis, package meta!
Translation_meta.ordered <- Translation_meta[order(-Translation_meta$Ratio_translation), ]
pes.forest <- metabin(Clin_positive, Clin_all, Pre_positive, Pre_all, Study, data = Translation_meta.ordered)
#equal_weight <- 1/length(pes.forest$TE)
#pes.forest$w.fixed <- rep(equal_weight, length(pes.forest$TE))
#pes.forest$w.random <- rep(equal_weight, length(pes.forest$TE))
png("forestplot_meta_translation_new_weigthequal.png", width = 5000, height = 4700, res = 300)
forest(pes.forest,
       label.e = "Clinical studies",
       label.c = "Animal studies",
       leftlabs = c("Study", "Beneficial outcome", "Total", "Beneficial outcome", "Total"),
       clab = "Prevalence", 
       xlab = "Translation ratio",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       comb.random = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3)
dev.off()

pes.forest$seTE
str(pes.forest)
A <- c(1.353, 1.145, 1.065, 1.000, 1.000, 1.000, 1.000, 0.943, 0.839, 0.846,
       0.818, 0.778, 0.769, 0.778, 0.762, 0.741, 0.739, 0.733, 0.636, 0.636,
       0.595, 0.523, 0.438, 0.429, 0.333, 0.165, 0.200, 0.163)
B <- c(5, 1.2, 12.2, 3.6, 0.6, 3.6, 4.1, 12.5, 6.5, 5.3,
       8.1, 4.2, 3.1, 6.3, 2.7, 1.2, 5.0, 3.6, 1.9, 1.9,
       4.0, 1.2, 0.3, 0.6, 0.8, 0.1, 0.1, 0.1)
test <- data.frame(A = A, B = B)
test$C <- (test$B)/100
test$D <- (test$A) * (test$C)
mean(test$D)*28
mean(test$A)
median(test$A)

##Forest plot, meta-analysis, package meta! for RCTs
while (!is.null(dev.list()))  dev.off()
Translation_meta.ordered <- Translation_meta[order(-Translation_meta$Ratio_translation_RCT), ]
pes.forest <- metabin(RCTs_positive, RCTs, Pre_positive, Pre_all, Study, data = Translation_meta.ordered)
png("forestplot_meta_translation_RCT_new.png", width = 5000, height = 4500, res = 300)
forest(pes.forest,
       label.e = "Randomized controlled trials",
       label.c = "Animal studies",
       leftlabs = c("Study", "Beneficial outcome", "Total", "Beneficial outcome", "Total"),
       clab = "Prevalence", 
       xlab = "Translation ratio RCT",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3)
dev.off()

##Forest plot, meta-analysis, package meta! for RCTs, second attempt
Translation_meta.ordered <- Translation_meta[order(-Translation_meta$Ratio_translation), ]
pes.forest <- metabin(Clin_positive_norct, Clin_all_norct, Pre_positive, Pre_all, Study, data = Translation_meta.ordered)
#equal_weight <- 1/length(pes.forest$TE)
#pes.forest$w.fixed <- rep(equal_weight, length(pes.forest$TE))
#pes.forest$w.random <- rep(equal_weight, length(pes.forest$TE))
png("forestplot_meta_translation_excludeRCTs.png", width = 5000, height = 4700, res = 300)
forest(pes.forest,
       label.e = "Clinical studies",
       label.c = "Animal studies",
       leftlabs = c("Study", "Beneficial outcome", "Total", "Beneficial outcome", "Total"),
       clab = "Prevalence", 
       xlab = "Translation ratio",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       comb.random = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3)
dev.off()



###With Haldane's correction
###########################
Haldane_RCT <- Translation_meta.ordered
columns_to_correct <- c("RCTs")

Haldane_RCT[, columns_to_correct] <- 
  Haldane_RCT[, columns_to_correct] + 0.001

pes.forest <- metabin(RCTs_positive, RCTs, Pre_positive, Pre_all, Study, data = Haldane_RCT)
png("forestplot_meta_translation_RCT_Haldane.png", width = 4200, height = 2502, res = 300)
forest(pes.forest,
       label.e = "Randomized controlled trials",
       label.c = "Animal studies",
       leftlabs = c("Study", "Beneficial outcome", "Total", "Beneficial outcome", "Total"),
       clab = "Prevalence", 
       xlab = "Translation ratio RCT",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3)
dev.off()

###calculate overall summary proportions, i.e., overall summary ES and subgroup ES
ies.logit <- escalc(xi = N_LME, ni = N, measure = "PLO", data = LME_MA)#Escalc: generate logit-transformed proportion and sampling variance. PLO=logit-transformation, PFT would be double arcsine transformation
pes.logit <- rma(yi, vi, method = "DL", weighted = TRUE, data = ies.logit)#DerSimonian-Laird
pes <- predict(pes.logit, transf = transf.ilogit)#convert logits back to proportions
print(pes, digits = 6)
print(pes.logit, digits = 4)
confint(pes.logit, digits = 2, level = 0.95)#confidence intervals of pes.logit

#####
###Subgroup analysis for different biomedical fields
Translation_meta_neuro <- subset(Translation_meta, Translation_meta$Disease_class1 == "Diseases of the nervous system"|
                             Translation_meta$Disease_class2 == "Diseases of the nervous system")

Translation_meta_neuro.ordered <- Translation_meta_neuro[order(-Translation_meta_neuro$Ratio_translation), ]
pes.forest <- metabin(Clin_positive, Clin_all, Pre_positive, Pre_all, Study, data = Translation_meta_neuro.ordered)
png("forestplot_meta_translation_neuro.png", width = 5000, height = 2200, res = 300)
forest(pes.forest,
       label.e = "Clinical studies",
       label.c = "Animal studies",
       leftlabs = c("Study", "Beneficial outcome", "Total", "Beneficial outcome", "Total"),
       clab = "Prevalence", 
       xlab = "Translation ratio",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3)
dev.off()

Translation_meta_psych <- subset(Translation_meta, Translation_meta$Disease_class1 == "Mental and behavioural disorders"|
                                   Translation_meta$Disease_class2 == "Mental and behavioural disorders")

Translation_meta_psych.ordered <- Translation_meta_psych[order(-Translation_meta_psych$Ratio_translation), ]
pes.forest <- metabin(Clin_positive, Clin_all, Pre_positive, Pre_all, Study, data = Translation_meta_psych.ordered)
png("forestplot_meta_translation_psych.png", width = 5000, height = 1200, res = 300)
forest(pes.forest,
       label.e = "Clinical studies",
       label.c = "Animal studies",
       leftlabs = c("Study", "Beneficial outcome", "Total", "Beneficial outcome", "Total"),
       clab = "Prevalence", 
       xlab = "Translation ratio",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3)
dev.off()

Translation_meta_circ <- subset(Translation_meta, Translation_meta$Disease_class1 == "Diseases of the circulatory system"|
                                   Translation_meta$Disease_class2 == "Diseases of the circulatory system")

Translation_meta_circ.ordered <- Translation_meta_circ[order(-Translation_meta_circ$Ratio_translation), ]
pes.forest <- metabin(Clin_positive, Clin_all, Pre_positive, Pre_all, Study, data = Translation_meta_circ.ordered)
png("forestplot_meta_translation_circ.png", width = 5000, height = 1500, res = 300)
forest(pes.forest,
       label.e = "Clinical studies",
       label.c = "Animal studies",
       leftlabs = c("Study", "Beneficial outcome", "Total", "Beneficial outcome", "Total"),
       clab = "Prevalence", 
       xlab = "Translation ratio",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3)
dev.off()

Translation_meta_digest <- subset(Translation_meta, Translation_meta$Disease_class1 == "Diseases of the digestive system"|
                                   Translation_meta$Disease_class2 == "Diseases of the digestive system")

Translation_meta_digest.ordered <- Translation_meta_digest[order(-Translation_meta_digest$Ratio_translation), ]
pes.forest <- metabin(Clin_positive, Clin_all, Pre_positive, Pre_all, Study, data = Translation_meta_digest.ordered)
png("forestplot_meta_translation_digest.png", width = 5000, height = 1200, res = 300)
forest(pes.forest,
       label.e = "Clinical studies",
       label.c = "Animal studies",
       leftlabs = c("Study", "Beneficial outcome", "Total", "Beneficial outcome", "Total"),
       clab = "Prevalence", 
       xlab = "Translation ratio",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3)
dev.off()

Translation_meta_musc <- subset(Translation_meta, Translation_meta$Disease_class1 == "Diseases of the musculskeletal system and connective tissue"|
                                   Translation_meta$Disease_class2 == "Diseases of the musculskeletal system and connective tissue")

Translation_meta_musc.ordered <- Translation_meta_musc[order(-Translation_meta_musc$Ratio_translation), ]
pes.forest <- metabin(Clin_positive, Clin_all, Pre_positive, Pre_all, Study, data = Translation_meta_musc.ordered)
png("forestplot_meta_translation_musc.png", width = 5000, height = 1100, res = 300)
forest(pes.forest,
       label.e = "Clinical studies",
       label.c = "Animal studies",
       leftlabs = c("Study", "Beneficial outcome", "Total", "Beneficial outcome", "Total"),
       clab = "Prevalence", 
       xlab = "Translation ratio",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3)
dev.off()

Translation_meta_cancer <- subset(Translation_meta, Translation_meta$Disease_class1 == "Neoplasms"|
                                   Translation_meta$Disease_class2 == "Neoplasms")

Translation_meta_cancer.ordered <- Translation_meta_cancer[order(-Translation_meta_cancer$Ratio_translation), ]
pes.forest <- metabin(Clin_positive, Clin_all, Pre_positive, Pre_all, Study, data = Translation_meta_cancer.ordered)
png("forestplot_meta_translation_cancer.png", width = 5000, height = 900, res = 300)
forest(pes.forest,
       label.e = "Clinical studies",
       label.c = "Animal studies",
       leftlabs = c("Study", "Beneficial outcome", "Total", "Beneficial outcome", "Total"),
       clab = "Prevalence", 
       xlab = "Translation ratio",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3)
dev.off()

###############################################
### Subgroup analyses for biomedical fields ###
###############################################
### Neuroscience
Translation_meta.ordered$Study
Neuro_row_names <- c("Ramos, 2022 (Epilepsy)", "Gruenbaum, 2019 (Epilepsy)",
                     "Murray, 2014 (Parkinson's disease)", "Jensen, 2014 (Multiple sclerosis)",
                     "Bhatti, 2017 (Traumatic brain injury)", "Lilamand, 2020 (Alzheimer's disease)",
                     "Voulgaropoulou, 2019 (Alzheimer's disease)", "Maskery, 2021 (Stroke)",
                     "Rendon, 2022 (Glioma/glioblastoma)")

# Subset the dataframe based on desired row names
Translation_meta.ordered.neuro <- Translation_meta.ordered[Translation_meta.ordered$Study %in% Neuro_row_names, ]

pes.forest <- metabin(Clin_positive, Clin_all, Pre_positive, Pre_all, Study, data = Translation_meta.ordered.neuro)
png("forestplot_meta_translation_RCT_Neuro.png", width = 4200, height = 2502, res = 300)
forest(pes.forest,
       label.e = "Randomized controlled trials",
       label.c = "Animal studies",
       leftlabs = c("Study", "Beneficial outcome", "Total", "Beneficial outcome", "Total"),
       clab = "Prevalence", 
       xlab = "Translation ratio RCT",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3)
dev.off()



### RCT proportion
Translation_meta.ordered$RCTs
Translation_meta.ordered$Clin_all

sum(Translation_meta.ordered$RCTs)/sum(Translation_meta.ordered$Clin_all)

sum(Translation_meta.ordered$Pre_positive)/sum(Translation_meta.ordered$Pre_all)
sum(Translation_meta.ordered$Clin_positive)/sum(Translation_meta.ordered$Clin_all)

median(Translation_meta.ordered$Clin_all)

#################
#### Countries ####
################
library(tidyr)
Countries <- read_excel("C:/Users/benja/Dropbox/Research/Experiments/NeuroscienceTranslation/Translation_manuscript/Mastersheet_extraction.xlsx", sheet = 3)

Countries_split <- Countries %>%
  separate_rows(Country, sep = ",\\s*")
Country_freq <- table(Countries_split$Country)
Country_freq_df <- as.data.frame(Country_freq, stringsAsFactors = FALSE)
colnames(Country_freq_df) <- c("Country", "Frequency")
Country_freq_df <- Country_freq_df[order(-Country_freq_df$Frequency), ]

Num_countries_per_row <- str_count(Countries$Country, ",") + 1

# Count the rows with more than one country
rows_with_multiple_countries <- sum(Num_countries_per_row > 1)


# Create a frequency table for the disease column
disease_freq_table <- table(Countries$Disease_grouped)

# Convert the frequency table to a data frame
disease_freq_df <- as.data.frame(disease_freq_table, stringsAsFactors = FALSE)

# Rename the columns for clarity
colnames(disease_freq_df) <- c("Disease", "Frequency")

# Sort the data frame by frequency in descending order
disease_freq_df <- disease_freq_df[order(-disease_freq_df$Frequency), ]

# Print the sorted data frame
print(disease_freq_df)


###########################
#### Translation rates ####
###########################
Translation_rates <- read_excel("C:/Users/benja/Dropbox/Research/Experiments/NeuroscienceTranslation/Translation_manuscript/Mastersheet_extraction.xlsx", sheet = 4)
Translation_rates <- subset(Translation_rates, Eligible_translation != "No")

#Observation years OVERALL
Translation_rates$Observation_years <- as.numeric(Translation_rates$Year) - as.numeric(Translation_rates$Year_firstanimal)
Translation_rates$Observation_years_2023 <- 2023 - as.numeric(Translation_rates$Year_firstanimal)
Translation_rates <- subset(Translation_rates, Translation_rates$Observation_years_2023 > 9)#Exclude line if all therapies are of interest

sum(Translation_rates$Observation_years_2023, na.rm = T)
median(Translation_rates$Observation_years_2023, na.rm = T)
range(Translation_rates$Observation_years_2023, na.rm = T)

#Number of therapies
Translation_rates$Therapy[Translation_rates$Therapy == "NA"] <- NA #replace "NA" with actual NAs
unique_sum <- sum(!is.na(Translation_rates$Therapy)) # Calculate the sum of unique therapies while disregarding NA values
print(unique_sum)
length(unique(Translation_rates$Therapy))#367 for ALL therapies (not considering development time of >9 years; see code line above)
length(unique(Translation_rates$Disease_grouped))

sum(Translation_rates$Human_trial == "Yes", na.rm = TRUE) #How many therapies entered a human trial
sum(Translation_rates$Human_trial == "No", na.rm = TRUE) #How many therapies did not enter a human trial

sum(Translation_rates$RCT == "Yes", na.rm = TRUE) #How many therapies entered an RCT
sum(Translation_rates$RCT == "No", na.rm = TRUE) #How many therapies did not enter an RCT

sum(Translation_rates$FDA_approval == "Yes", na.rm = TRUE) #How many therapies were FDA-approved
sum(Translation_rates$FDA_approval == "No", na.rm = TRUE) #How many therapies were not FDA-approved

#Therapeutic interventions for unique diseases
length(unique(Translation_rates$Therapy[Translation_rates$Disease == "Laparoscopic liver surgery, robotic surgery"]))#Add disease/condition of interest
sum(Translation_rates$Disease == "Laparoscopic liver surgery, robotic surgery" & Translation_rates$Human_trial == "Yes", na.rm = TRUE)
sum(Translation_rates$Disease == "Laparoscopic liver surgery, robotic surgery" & Translation_rates$RCT == "Yes", na.rm = TRUE)
sum(Translation_rates$Disease == "Laparoscopic liver surgery, robotic surgery" & Translation_rates$FDA_approval == "Yes", na.rm = TRUE)

#Proportion of drugs entering a human trial, an RCT or were FDA-approved OVERALL
Humantrial_overall <- round((sum(Translation_rates$Human_trial == "Yes", na.rm = TRUE) / unique_sum) * 100, 0)
RCT_overall <- round((sum(Translation_rates$RCT == "Yes", na.rm = TRUE)/unique_sum)*100, 0)
FDA_overall <- round((sum(Translation_rates$FDA_approval == "Yes", na.rm = TRUE)/unique_sum)*100, 0)

Translation_overall <- data.frame(
  Variable = factor(c("Clinical trial", "RCT", "(FDA) approval"),
                    levels = c("Clinical trial", "RCT", "(FDA) approval")),
  Value = c(Humantrial_overall, RCT_overall, FDA_overall)
)

#Value2 = c(paste0(Humantrial_overall, " %"), paste0(RCT_overall, " %"), paste0(FDA_overall, " %"))


custom_colors <- c("Clinical trial" = "#759311", "RCT" = "#FECA15", "(FDA) approval" = "#D73C1F")

Translation_overall_bar <- ggplot(Translation_overall, aes(x = reorder(Variable, -Value), y = Value, fill = Variable)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 4) +  # Add value labels
  labs(title = "A. Translational proportions overall",
       x = "", y = "Percentage (%)") +
  scale_y_continuous(limits = c(0, 110), breaks = c(0, 25, 50, 75, 100)) +  # Set y-axis limits
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),  # Adjust axis tick font size
    axis.title.y = element_text(size = 14),
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")  # Adjust title font size and appearance
  )


###Subgroup analyses###
########################

#Neuroscience
#############
Translation_rates_neuroscience <- subset(Translation_rates, Translation_rates$Disease_class1 == "Diseases of the nervous system"|
                                           Translation_rates$Disease_class2 == "Diseases of the nervous system")

#How many therapies
unique_sum_neuro <- sum(!is.na(Translation_rates_neuroscience$Therapy)) # Calculate the sum of unique therapies while disregarding NA values
print(unique_sum_neuro)
length(unique(Translation_rates$Therapy))

#How many trials
sum(Translation_rates_neuroscience$Observation_years_2023, na.rm = T)
median(Translation_rates_neuroscience$Observation_years_2023, na.rm = T)
range(Translation_rates_neuroscience$Observation_years_2023, na.rm = T)

#Create dataframe with translational rates
Humantrial_neuro <- round((sum(Translation_rates_neuroscience$Human_trial == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_neuroscience$Therapy)))*100, 0)
RCT_neuro <- round((sum(Translation_rates_neuroscience$RCT == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_neuroscience$Therapy)))*100, 0)
FDA_neuro <- round((sum(Translation_rates_neuroscience$FDA_approval == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_neuroscience$Therapy)))*100, 0)

#Chart with translational rates
Translation_neuro <- data.frame(
  Variable = factor(c("Clinical trial", "RCT", "(FDA) approval"),
                    levels = c("Clinical trial", "RCT", "(FDA) approval")),
  Value = c(Humantrial_neuro, RCT_neuro, FDA_neuro)
)

Translation_neuro_bar <- ggplot(Translation_neuro, aes(x = reorder(Variable, -Value), y = Value, fill = Variable)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 4) +  # Add value labels
  labs(title = "C. Nervous system diseases",
       x = "", y = "Percentage (%)") +
  scale_y_continuous(limits = c(0, 110), breaks = c(0, 25, 50, 75, 100)) +  # Set y-axis limits
  scale_fill_manual(values = c("#8c8c8c", "#B2B2B2", "#D9D9D9")) +  # Set custom colors
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),  # Adjust axis tick font size
    axis.title.y = element_text(size = 14),
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")  # Adjust title font size and appearance
  )

#######################
#Mental health disorders
########################
Translation_rates_psych <- subset(Translation_rates, Translation_rates$Disease_class1 == "Mental and behavioural disorders"|
                                           Translation_rates$Disease_class2 == "Mental and behavioural disorders")

#How many therapies
unique_sum_psych <- sum(!is.na(Translation_rates_psych$Therapy)) # Calculate the sum of unique therapies while disregarding NA values
print(unique_sum_psych)
length(unique(Translation_rates_psych$Therapy))

#How many trials
sum(Translation_rates_psych$Observation_years_2023, na.rm = T)
median(Translation_rates_psych$Observation_years_2023, na.rm = T)
range(Translation_rates_psych$Observation_years_2023, na.rm = T)

#Create dataframe with translational rates
Humantrial_psych <- round((sum(Translation_rates_psych$Human_trial == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_psych$Therapy)))*100, 0)
RCT_psych <- round((sum(Translation_rates_psych$RCT == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_psych$Therapy)))*100, 0)
FDA_psych <- round((sum(Translation_rates_psych$FDA_approval == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_psych$Therapy)))*100, 0)

#Chart with translational rates
Translation_psych <- data.frame(
  Variable = factor(c("Clinical trial", "RCT", "(FDA) approval"),
                    levels = c("Clinical trial", "RCT", "(FDA) approval")),
  Value = c(Humantrial_psych, RCT_psych, FDA_psych)
)

Translation_psych_bar <- ggplot(Translation_psych, aes(x = reorder(Variable, -Value), y = Value, fill = Variable)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 4) +  # Add value labels
  labs(title = "D. Mental health disorders",
       x = "", y = "Percentage (%)") +
  scale_y_continuous(limits = c(0, 110), breaks = c(0, 25, 50, 75, 100)) +  # Set y-axis limits
  scale_fill_manual(values = c("#8c8c8c", "#B2B2B2", "#D9D9D9")) +  # Set custom colors
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),  # Adjust axis tick font size
    axis.title.y = element_text(size = 14),
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")  # Adjust title font size and appearance
  )

#######################
#Diseases of the digestive system
########################
Translation_rates_digest <- subset(Translation_rates, Translation_rates$Disease_class1 == "Diseases of the digestive system"|
                                    Translation_rates$Disease_class2 == "Diseases of the digestive system")

#How many therapies
unique_sum_digest <- sum(!is.na(Translation_rates_digest$Therapy)) # Calculate the sum of unique therapies while disregarding NA values
print(unique_sum_digest)
length(unique(Translation_rates$Therapy))

#How many trials
sum(Translation_rates_digest$Observation_years_2023, na.rm = T)
median(Translation_rates_digest$Observation_years_2023, na.rm = T)
range(Translation_rates_digest$Observation_years_2023, na.rm = T)

#Create dataframe with translational rates
Humantrial_digest <- round((sum(Translation_rates_digest$Human_trial == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_digest$Therapy)))*100, 0)
RCT_digest <- round((sum(Translation_rates_digest$RCT == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_digest$Therapy)))*100, 0)
FDA_digest <- round((sum(Translation_rates_digest$FDA_approval == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_digest$Therapy)))*100, 0)

#Chart with translational rates
Translation_digest <- data.frame(
  Variable = factor(c("Clinical trial", "RCT", "(FDA) approval"),
                    levels = c("Clinical trial", "RCT", "(FDA) approval")),
  Value = c(Humantrial_digest, RCT_digest, FDA_digest)
)

Translation_digest_bar <- ggplot(Translation_digest, aes(x = reorder(Variable, -Value), y = Value, fill = Variable)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 4) +  # Add value labels
  labs(title = "B. Digestive system diseases",
       x = "", y = "Percentage (%)") +
  scale_y_continuous(limits = c(0, 110), breaks = c(0, 25, 50, 75, 100)) +  # Set y-axis limits
  scale_fill_manual(values = c("#8c8c8c", "#B2B2B2", "#D9D9D9")) +  # Set custom colors
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),  # Adjust axis tick font size
    axis.title.y = element_text(size = 14),
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")  # Adjust title font size and appearance
  )

#######################
#Diseases of the musculoskeletal system
########################
Translation_rates_musc <- subset(Translation_rates, Translation_rates$Disease_class1 == "Diseases of the musculskeletal system and connective tissue"|
                                     Translation_rates$Disease_class2 == "Diseases of the musculskeletal system and connective tissue")

#How many therapies
unique_sum_musc <- sum(!is.na(Translation_rates_musc$Therapy)) # Calculate the sum of unique therapies while disregarding NA values
print(unique_sum_musc)
length(unique(Translation_rates_musc$Therapy))

#How many trials
sum(Translation_rates_musc$Observation_years_2023, na.rm = T)
median(Translation_rates_musc$Observation_years_2023, na.rm = T)
range(Translation_rates_musc$Observation_years_2023, na.rm = T)

#Create dataframe with translational rates
Humantrial_musc <- round((sum(Translation_rates_musc$Human_trial == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_musc$Therapy)))*100, 0)
RCT_musc <- round((sum(Translation_rates_musc$RCT == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_musc$Therapy)))*100, 0)
FDA_musc <- round((sum(Translation_rates_musc$FDA_approval == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_musc$Therapy)))*100, 0)

#Chart with translational rates
Translation_musc <- data.frame(
  Variable = factor(c("Clinical trial", "RCT", "(FDA) approval"),
                    levels = c("Clinical trial", "RCT", "(FDA) approval")),
  Value = c(Humantrial_musc, RCT_musc, FDA_musc)
)

Translation_musc_bar <- ggplot(Translation_musc, aes(x = reorder(Variable, -Value), y = Value, fill = Variable)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 4) +  # Add value labels
  labs(title = "E. Musculoskeletal diseases",
       x = "", y = "Percentage (%)") +
  scale_y_continuous(limits = c(0, 110), breaks = c(0, 25, 50, 75, 100)) +  # Set y-axis limits
  scale_fill_manual(values = c("#8c8c8c", "#B2B2B2", "#D9D9D9")) +  # Set custom colors
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),  # Adjust axis tick font size
    axis.title.y = element_text(size = 14),
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")  # Adjust title font size and appearance
  )


#######################
#Diseases of the circulatory system
########################
Translation_rates_circ <- subset(Translation_rates, Translation_rates$Disease_class1 == "Diseases of the circulatory system"|
                                   Translation_rates$Disease_class2 == "Diseases of the circulatory system")

#How many therapies
unique_sum_circ <- sum(!is.na(Translation_rates_circ$Therapy)) # Calculate the sum of unique therapies while disregarding NA values
print(unique_sum_circ)
length(unique(Translation_rates_circ$Therapy))

#How many trials
sum(Translation_rates_circ$Observation_years_2023, na.rm = T)
median(Translation_rates_circ$Observation_years_2023, na.rm = T)
range(Translation_rates_circ$Observation_years_2023, na.rm = T)

#Create dataframe with translational rates
Humantrial_circ <- round((sum(Translation_rates_circ$Human_trial == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_circ$Therapy)))*100, 0)
RCT_circ <- round((sum(Translation_rates_circ$RCT == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_circ$Therapy)))*100, 0)
FDA_circ <- round((sum(Translation_rates_circ$FDA_approval == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_circ$Therapy)))*100, 0)

#Chart with translational rates
Translation_circ <- data.frame(
  Variable = factor(c("Clinical trial", "RCT", "(FDA) approval"),
                    levels = c("Clinical trial", "RCT", "(FDA) approval")),
  Value = c(Humantrial_circ, RCT_circ, FDA_circ)
)

Translation_circ_bar <- ggplot(Translation_circ, aes(x = reorder(Variable, -Value), y = Value, fill = Variable)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 4) +  # Add value labels
  labs(title = "B. Circulatory system diseases",
       x = "", y = "Percentage (%)") +
  scale_y_continuous(limits = c(0, 110), breaks = c(0, 25, 50, 75, 100)) +  # Set y-axis limits
  scale_fill_manual(values = c("#8c8c8c", "#B2B2B2", "#D9D9D9")) +  # Set custom colors
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),  # Adjust axis tick font size
    axis.title.y = element_text(size = 14),
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")  # Adjust title font size and appearance
  )

#######################
#Neoplasms
########################
Translation_rates_cancer <- subset(Translation_rates, Translation_rates$Disease_class1 == "Neoplasms"|
                                   Translation_rates$Disease_class2 == "Neoplasms")

#How many therapies
unique_sum_cancer <- sum(!is.na(Translation_rates_cancer$Therapy)) # Calculate the sum of unique therapies while disregarding NA values
print(unique_sum_cancer)
length(unique(Translation_rates_cancer$Therapy))

#How many trials
sum(Translation_rates_cancer$Observation_years_2023, na.rm = T)
median(Translation_rates_cancer$Observation_years_2023, na.rm = T)
range(Translation_rates_cancer$Observation_years_2023, na.rm = T)

#Create dataframe with translational rates
Humantrial_cancer <- round((sum(Translation_rates_cancer$Human_trial == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_cancer$Therapy)))*100, 0)
RCT_cancer <- round((sum(Translation_rates_cancer$RCT == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_cancer$Therapy)))*100, 0)
FDA_cancer <- round((sum(Translation_rates_cancer$FDA_approval == "Yes", na.rm = TRUE)/sum(!is.na(Translation_rates_cancer$Therapy)))*100, 0)

#Chart with translational rates
Translation_cancer <- data.frame(
  Variable = factor(c("Clinical trial", "RCT", "(FDA) approval"),
                    levels = c("Clinical trial", "RCT", "(FDA) approval")),
  Value = c(Humantrial_cancer, RCT_cancer, FDA_cancer)
)

Translation_cancer_bar <- ggplot(Translation_cancer, aes(x = reorder(Variable, -Value), y = Value, fill = Variable)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 4) +  # Add value labels
  labs(title = "F. Cancer",
       x = "", y = "Percentage (%)") +
  scale_y_continuous(limits = c(0, 110), breaks = c(0, 25, 50, 75, 100)) +  # Set y-axis limits
  scale_fill_manual(values = c("#8c8c8c", "#B2B2B2", "#D9D9D9")) +  # Set custom colors
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),  # Adjust axis tick font size
    axis.title.y = element_text(size = 14),
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")  # Adjust title font size and appearance
  )


### Arrange plots for translational rates
library(gridExtra)


grid_arrangement <- grid.arrange(
  arrangeGrob(Translation_overall_bar, Translation_circ_bar, ncol = 2),
  arrangeGrob(Translation_neuro_bar, Translation_psych_bar, ncol = 2),
  arrangeGrob(Translation_musc_bar, Translation_cancer_bar, ncol = 2),
  ncol = 1,
  heights = c(2, 2, 2)
)

print(grid_arrangement)  # This will display the combined plot

#Print figure
png("Translation_plot_all.V2.png", width=2300, height=2800, res=300)
grid.arrange(
  arrangeGrob(Translation_overall_bar, Translation_circ_bar, ncol = 2),
  arrangeGrob(Translation_neuro_bar, Translation_psych_bar, ncol = 2),
  arrangeGrob(Translation_musc_bar, Translation_cancer_bar, ncol = 2),
  ncol = 1,
  heights = c(2, 2, 2)
)
dev.off()

#####################################
##### Translation per disease  ######
#####################################
Translation_disease <- read_excel("C:/Users/benja/Dropbox/Research/Experiments/NeuroscienceTranslation/Translation_manuscript/Mastersheet_extraction.xlsx", sheet = 6)

# Create a numeric index based on the order of Therapy_No
Translation_disease$Gradient_Index <- seq_along(Translation_disease$Therapy_No)

# Determine the order of 'Disease' based on 'Therapy_No'
disease_order <- Translation_disease$Disease[order(-Translation_disease$Therapy_No)]

# Now plot the bar chart with a one-color gradient effect
ggplot(Translation_disease, aes(x = reorder(Disease, -Therapy_No), y = Therapy_No, fill = Gradient_Index)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Adjust the colors for the gradient
  theme_minimal() +
  labs(y = "Number of therapies investigated", x = NULL) +
  coord_cartesian(ylim = c(0, 50)) +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        legend.position = "none",
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold"))

# Create the second graph with the same order and include disease names on the x-axis
ggplot(Translation_disease, aes(x = factor(Disease, levels = disease_order), y = Any_human, fill = Gradient_Index)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#ccff99", high = "darkgreen") +  # Adjust the colors for the gradient
  theme_minimal() +
  labs(y = "Number of therapies entering any human trial", x = NULL) +
  coord_cartesian(ylim = c(0, max(Translation_disease$Any_human))) + # Adjust y-axis limits based on 'Any_human'
  theme(axis.text.x = element_blank(),  # Add disease names on the x-axis
        legend.position = "none",
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold"))

# Create the second graph with the same order and include disease names on the x-axis
ggplot(Translation_disease, aes(x = factor(Disease, levels = disease_order), y = RCT, fill = Gradient_Index)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#ffff99", high = "orange") +  # Adjust the colors for the gradient
  theme_minimal() +
  labs(y = "Number of therapies entering an RCT", x = NULL) +
  coord_cartesian(ylim = c(0, 30)) + # Adjust y-axis limits based on 'Any_human'
  theme(axis.text.x = element_blank(),  # Add disease names on the x-axis
        legend.position = "none",
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold"))

# Create the second graph with the same order and include disease names on the x-axis
ggplot(Translation_disease, aes(x = factor(Disease, levels = disease_order), y = FDA, fill = Gradient_Index)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#feb9b9", high = "#fc0000") +  # Adjust the colors for the gradient
  theme_minimal() +
  labs(y = "With (FDA) approval", x = NULL) +
  coord_cartesian(ylim = c(0, 30)) + # Adjust y-axis limits based on 'Any_human'
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Add disease names on the x-axis
        legend.position = "none",
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold"))

###########################
#### Survival analysis ####
###########################
library(survival)
library(survminer)
library(grid)

Translation_survival <- read_excel("C:/Users/benja/Dropbox/Research/Experiments/NeuroscienceTranslation/Translation_manuscript/Mastersheet_extraction.xlsx", sheet = 4)
sum(Translation_survival$Human_trial == "Yes", na.rm = T)

###########################
#Observation years to clinical trial##

Translation_survival_clin <- subset(Translation_survival, Human_trial == "Yes")
Translation_survival_clin$tte_clinicaltrial <- as.numeric(Translation_survival_clin$Year_humantrial) - as.numeric(Translation_survival_clin$Year_firstanimal)
sum(Translation_survival_clin$tte_clinicaltrial < 0)
#Translation_survival_clin <- subset(Translation_survival_clin, tte_clinicaltrial > 0)
367-length(Translation_survival_clin$tte_clinicaltrial)#202

colnames(Translation_survival_clin)
selected_columns <- c("Study", "Disease_grouped", "Therapy", "tte_clinicaltrial")
Translation_survival_clin <- Translation_survival_clin[selected_columns]# Subset the dataframe to keep only the selected columns
Translation_survival_clin$event <- 1

# Define the number of rows to add
num_rows_to_add <- 202

# Create a data frame with 256 rows and the same column names as your existing data frame
new_rows <- data.frame(
  Study = rep(NA, num_rows_to_add),
  Disease_grouped = rep(NA, num_rows_to_add),
  Therapy = rep(NA, num_rows_to_add),
  tte_clinicaltrial = rep(NA, num_rows_to_add),
  event = rep(NA, num_rows_to_add)
)

# Add the new rows to the existing data frame (only use this codeline if you want to include all therapies, even those without translation. Deactivate the code if you only want the translated therapies)
#Translation_survival_clin <- rbind(Translation_survival_clin, new_rows)

Translation_survival_clin$tte_clinicaltrial <- ifelse(Translation_survival_clin$tte_clinicaltrial < 0, 0, Translation_survival_clin$tte_clinicaltrial)
Translation_survival_clin$event <- ifelse(is.na(Translation_survival_clin$event), 0, Translation_survival_clin$event)
Translation_survival_clin$tte_clinicaltrial <- ifelse(is.na(Translation_survival_clin$tte_clinicaltrial),
                                          max(Translation_survival_clin$tte_clinicaltrial, na.rm = TRUE),
       
                                                                             Translation_survival_clin$tte_clinicaltrial)

#Deactivate these two lines if you want to include ALL therapies, i.e., not only the ones with positive tte (supplementary figure 2)
sum(Translation_survival_clin$tte_clinicaltrial == 0)#54 with a 0 as time-to-event: 165-54=111
Translation_survival_clin <- subset(Translation_survival_clin, tte_clinicaltrial > 0)#subset for only tte > 0

#Model fit
model_fit_clin <- survfit(Surv(tte_clinicaltrial, event) ~ 1, data = Translation_survival_clin, type = "kaplan-meier")
summary(model_fit_clin)$table

# Create the survival plot without the risk table
surv_plot <- ggsurvplot(model_fit_clin,
                        pval = F, conf.int = TRUE,
                        risk.table = FALSE,
                        linetype = "strata",
                        surv.median.line = "hv",
                        ggtheme = theme_minimal(),
                        palette = "#2E9FDF",
                        legend = "none",
                        font.tickslab = c(14, "plain", "black"))

surv_plot$plot <- surv_plot$plot + 
  labs(title = "A. First animal study to first human study",
       x = " \n Time (years)",
       y = "\n Survival probability \n") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold", hjust = 0)) +
        scale_x_continuous(breaks = seq(0, 60, by = 5)) +  # Replace max_time with the maximum value of your data
        coord_cartesian(xlim = c(0, 60))

# Create the risk table plot with adjusted font
risk_table_plot <- ggsurvplot(model_fit_clin,
                              risk.table = TRUE,
                              ggtheme = theme_minimal(),
                              font.tickslab = c(16, "plain", "black"),
                              risk.table.fontsize = 6)  # Adjust font size for the risk table

risk_table_plot$table <- risk_table_plot$table + 
  labs(y = "\n Strata \n \n", x = "\n Time (years)") + 
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
        scale_x_continuous(breaks = seq(0, 60, by = 5)) + # Replace max_time with the maximum value of your data
        coord_cartesian(xlim = c(0, 60))

# Combine and display
Clin_survival <- grid.arrange(surv_plot$plot, risk_table_plot$table, ncol = 1, heights = c(2, 1))

###########################
#Observation years to RCT##

Translation_survival_rct <- subset(Translation_survival, RCT == "Yes")
Translation_survival_rct$tte_rct <- as.numeric(Translation_survival_rct$Year_RCT) - as.numeric(Translation_survival_rct$Year_firstanimal)
#Translation_survival_rct <- subset(Translation_survival_rct, tte_rct > 0)
sum(Translation_survival_rct$tte_rct < 0)

colnames(Translation_survival_rct)
selected_columns <- c("Study", "Disease_grouped", "Therapy", "tte_rct")
Translation_survival_rct <- Translation_survival_rct[selected_columns]# Subset the dataframe to keep only the selected columns
Translation_survival_rct$event <- 1
367-length(Translation_survival_rct$tte_rct)#235

# Define the number of rows to add
num_rows_to_add <- 235

# Create a data frame with 256 rows and the same column names as your existing data frame
new_rows <- data.frame(
  Study = rep(NA, num_rows_to_add),
  Disease_grouped = rep(NA, num_rows_to_add),
  Therapy = rep(NA, num_rows_to_add),
  tte_rct = rep(NA, num_rows_to_add),
  event = rep(NA, num_rows_to_add)
)

# Add the new rows to the existing data frame
#Translation_survival_rct <- rbind(Translation_survival_rct, new_rows)

Translation_survival_rct$tte_rct <- ifelse(Translation_survival_rct$tte_rct < 0, 0, Translation_survival_rct$tte_rct)
Translation_survival_rct$event <- ifelse(is.na(Translation_survival_rct$event), 0, Translation_survival_rct$event)
Translation_survival_rct$tte_rct <- ifelse(is.na(Translation_survival_rct$tte_rct),
                                                      max(Translation_survival_rct$tte_rct, na.rm = TRUE),
                                                     Translation_survival_rct$tte_rct)

#Deactivate these two lines if you want to include ALL therapies, i.e., not only the ones with positive tte (supplementary figure 2)
sum(Translation_survival_rct$tte_rct == 0)#32
Translation_survival_rct <- subset(Translation_survival_rct, tte_rct > 0)#subset for only tte > 0

#Model fit
model_fit_rct <- survfit(Surv(tte_rct, event) ~ 1, data = Translation_survival_rct, type = "kaplan-meier")
summary(model_fit_rct)$table

# Create the survival plot without the risk table
surv_plot <- ggsurvplot(model_fit_rct,
                        pval = F, conf.int = TRUE,
                        risk.table = FALSE,
                        linetype = "strata",
                        surv.median.line = "hv",
                        ggtheme = theme_minimal(),
                        palette = "#2E9FDF",
                        legend = "none",
                        font.tickslab = c(14, "plain", "black"))

surv_plot$plot <- surv_plot$plot + 
  labs(title = "B. First animal study to first RCT",
       x = " \n Time (years)",
       y = "\n Survival probability \n") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold", hjust = 0)) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +  # Replace max_time with the maximum value of your data
  coord_cartesian(xlim = c(0, 60))

# Create the risk table plot with adjusted font
risk_table_plot <- ggsurvplot(model_fit_rct,
                              risk.table = TRUE,
                              ggtheme = theme_minimal(),
                              font.tickslab = c(16, "plain", "black"),
                              risk.table.fontsize = 6)  # Adjust font size for the risk table

risk_table_plot$table <- risk_table_plot$table + 
  labs(y = "\n Strata \n \n", x = "\n Time (years)") + 
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) + # Replace max_time with the maximum value of your data
  coord_cartesian(xlim = c(0, 60))
# Combine and display
RCT_survival <- grid.arrange(surv_plot$plot, risk_table_plot$table, ncol = 1, heights = c(2, 1))


###########################
#Observation years to FDA##
Translation_survival_fda <- subset(Translation_survival, FDA_approval == "Yes")
Translation_survival_fda$tte_fda <- as.numeric(Translation_survival_fda$Year_FDA) - as.numeric(Translation_survival_fda$Year_firstanimal)
#Translation_survival_fda <- subset(Translation_survival_fda, tte_fda > 0)

colnames(Translation_survival_fda)
selected_columns <- c("Study", "Disease_grouped", "Therapy", "tte_fda")
Translation_survival_fda <- Translation_survival_fda[selected_columns]# Subset the dataframe to keep only the selected columns
Translation_survival_fda$event <- 1
length(Translation_survival_rct$event[Translation_survival_rct$event == 1])#No of RCTs

# Define the number of rows to add
num_rows_to_add <- length(Translation_survival_rct$event[Translation_survival_rct$event == 1]) - length(Translation_survival_fda$tte_fda)


# Create a data frame with 256 rows and the same column names as your existing data frame
new_rows <- data.frame(
  Study = rep(NA, num_rows_to_add),
  Disease_grouped = rep(NA, num_rows_to_add),
  Therapy = rep(NA, num_rows_to_add),
  tte_fda = rep(NA, num_rows_to_add),
  event = rep(NA, num_rows_to_add)
)

# Add the new rows to the existing data frame
#Translation_survival_fda <- rbind(Translation_survival_fda, new_rows)

Translation_survival_fda$tte_fda <- ifelse(Translation_survival_fda$tte_fda < 0, 0, Translation_survival_fda$tte_fda)
Translation_survival_fda$event <- ifelse(is.na(Translation_survival_fda$event), 0, Translation_survival_fda$event)
Translation_survival_fda$tte_fda <- ifelse(is.na(Translation_survival_fda$tte_fda),
                                           max(Translation_survival_fda$tte_fda, na.rm = TRUE),
                                           Translation_survival_fda$tte_fda)

#Deactivate these two lines if you want to include ALL therapies, i.e., not only the ones with positive tte (supplementary figure 2)
sum(Translation_survival_fda$tte_fda == 0)#32
Translation_survival_fda <- subset(Translation_survival_fda, tte_fda > 0)#subset for only tte > 0

model_fit_fda <- survfit(Surv(tte_fda, event) ~ 1, data = Translation_survival_fda, type = "kaplan-meier")
summary(model_fit_fda)$table

# Create the survival plot without the risk table
surv_plot <- ggsurvplot(model_fit_fda,
                        pval = F, conf.int = TRUE,
                        risk.table = FALSE,
                        linetype = "strata",
                        surv.median.line = "hv",
                        ggtheme = theme_minimal(),
                        palette = "#2E9FDF",
                        legend = "none",
                        font.tickslab = c(14, "plain", "black"))

surv_plot$plot <- surv_plot$plot + 
  labs(title = "C. First animal study to (FDA) approval",
       x = " \n Time (years)",
       y = "\n Survival probability \n") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold", hjust = 0)) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +  # Replace max_time with the maximum value of your data
  coord_cartesian(xlim = c(0, 60))

# Create the risk table plot with adjusted font
risk_table_plot <- ggsurvplot(model_fit_fda,
                              risk.table = TRUE,
                              ggtheme = theme_minimal(),
                              font.tickslab = c(16, "plain", "black"),
                              risk.table.fontsize = 6)  # Adjust font size for the risk table

risk_table_plot$table <- risk_table_plot$table + 
  labs(y = "\n Strata \n \n", x = "\n Time (years)") + 
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +  # Replace max_time with the maximum value of your data
  coord_cartesian(xlim = c(0, 60))

# Combine and display
FDA_survival <- grid.arrange(surv_plot$plot, risk_table_plot$table, ncol = 1, heights = c(2, 1))



grid_arrangement_survival <- grid.arrange(
  Clin_survival,
  RCT_survival,
  FDA_survival,
  ncol = 1
)

#Print figure
png("Survival_plot_V2.png", width=2000, height=4000, res=300)
grid.arrange(
  Clin_survival,
  RCT_survival,
  FDA_survival,
  ncol = 1
)
dev.off()


#######
## Treatment table
#####
Journey <- read_excel("C:/Users/benja/Dropbox/Research/Experiments/NeuroscienceTranslation/Translation_manuscript/Mastersheet_extraction.xlsx", sheet = 5)#It was sheet 2 formerly

length(unique(Journey$Therapy))# number of therapies


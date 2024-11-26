# Load packages

library(readxl)
library(tidyverse)
library(ggplot2)

# Read in group data

groupA = read_xlsx("data/groupAinfantsData_AD_23Feb2023.xlsx")
varA = colnames(groupA[, 17:56])

groupB = read_xlsx("data/groupBinfantsData_AD_23Feb2023.xlsx")
varB = colnames(groupB[, 16:34])

# Change variable names

## Study design and covariates

colnames(groupA)[1:4] = c("ID", "Group", "Time", "Treatment")
groupA$Time = as.factor(groupA$Time)
colnames(groupA)[10] = c("QFT")
colnames(groupA)[12:13] = c("Feeding", "Antibiotic")

colnames(groupB)[1:4] =  c("ID", "Group", "Time", "Treatment")
groupB$Time = as.factor(groupB$Time)
colnames(groupB)[10] = c("QFT")
colnames(groupB)[12:13] = c("Feeding", "Antibiotic")

# Create new dummy variables for feeding and antibiotic

groupA$FA = ifelse(groupA$Feeding == "Breast Milk" & groupA$Antibiotic == "Yes", 3, ifelse(groupA$Feeding == "Breast Milk" & groupA$Antibiotic == "No", 2, ifelse(groupA$Feeding == "Formula" & groupA$Antibiotic == "No", 1, 0)))
groupA$FA = factor(groupA$FA)

groupB$FA = ifelse(groupB$Feeding == "Breast Milk" & groupB$Antibiotic == "Y", 3, ifelse(groupB$Feeding == "Breast Milk" & groupB$Antibiotic == "N", 2, ifelse(groupB$Feeding == "Formula" & groupB$Antibiotic == "N", 1, 0)))
groupB$FA = factor(groupB$FA)

## Create immune response data sets

# Rename by number

groupA = rename_with(.data = groupA, .fn = ~ paste0("O", 1:length(varA)), .cols = 17:56)
groupB = rename_with(.data = groupB, .fn = ~ paste0("O", 1:length(varB)), .cols = 16:34)

# Drop indeterminate QFT observations
indet_idsA = groupA[which(groupA$QFT == "Indeterminate"), ]$ID
indet_idsB = groupB[which(groupB$QFT == "Indeterminate"), ]$ID

groupA = groupA[-which(groupA$ID %in% indet_idsA), ]
groupB = groupB[-which(groupB$ID %in% indet_idsB), ]




# Load packages

library(xtable)
library(kableExtra)
library(dplyr)
library(reshape2)
library(knitr)

# Keep only the first observation per patient - variables of interest do not change over time, only immune outcomes

groupAstats = groupA %>% group_by(ID) %>% slice(1) %>% ungroup()
groupBstats = groupB %>% group_by(ID) %>% slice(1) %>% ungroup()

# MVA85A

table_2way1 = table(groupAstats$Group, groupAstats$Treatment)
table_2way2 = table(groupBstats$Group, groupBstats$Treatment)

# Convert the table to a data frame for better handling
table_2way1_df = as.data.frame(rbind(table_2way1, table_2way2))
table_2way1_df$Total = apply(table_2way1_df, 1, sum)

# Calculate row percentages
table_2way1_percent = table_2way1_df %>%
  mutate(Control_Percent = Control / Total * 100,
         MVA85A_Percent = MVA85A / Total * 100)

# Display counts with percentages in a combined format
table_2way1_combined = table_2way1_percent %>%
  mutate(Control = paste0(as.character(Control), " (", round(Control_Percent, 1), "%)"),
         MVA85A = paste0(as.character(MVA85A), " (", round(MVA85A_Percent, 1), "%)"),
         Total = paste0(as.character(Total), " (100%)"))

table_2way1_combined = table_2way1_combined[, 1:3]

table_2way1_combined %>%
  kbl(format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

# QFT

table_2way1 = table(groupAstats$Group, groupAstats$QFT)
table_2way2 = table(groupBstats$Group, groupBstats$QFT)

# Convert the table to a data frame for better handling
table_2way2_df = as.data.frame(rbind(table_2way1, table_2way2))
table_2way2_df$Total = apply(table_2way2_df, 1, sum)

# Calculate row percentages
table_2way2_percent = table_2way2_df %>%
  mutate(Negative_Percent = Negative / Total * 100,
         Positive_Percent = Positive / Total * 100)

# Display counts with percentages in a combined format
table_2way2_combined = table_2way2_percent %>%
  mutate(Negative = paste0(as.character(Negative), " (", round(Negative_Percent, 1), "%)"),
         Positive = paste0(as.character(Positive), " (", round(Positive_Percent, 1), "%)"),
         Total = paste0(as.character(Total), " (100%)"))

table_2way2_combined = table_2way2_combined[, 1:3]

table_2way2_combined %>%
  kbl(format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

# Feeding

table_2way1 = table(groupAstats$Group, groupAstats$Feeding)
table_2way2 = table(groupBstats$Group, groupBstats$Feeding)

# Convert the table to a data frame for better handling
table_2way3_df = as.data.frame(rbind(table_2way1, table_2way2))
table_2way3_df$Total = apply(table_2way3_df, 1, sum)

# Calculate row percentages
table_2way3_percent = table_2way3_df %>%
  mutate(Breast_Percent = `Breast Milk` / Total * 100,
         Formula_Percent = Formula / Total * 100)

# Display counts with percentages in a combined format
table_2way3_combined = table_2way3_percent %>%
  mutate(`Breast Milk` = paste0(as.character(`Breast Milk`), " (", round(Breast_Percent, 1), "%)"),
         Formula = paste0(as.character(Formula), " (", round(Formula_Percent, 1), "%)"),
         Total = paste0(as.character(Total), " (100%)"))

table_2way3_combined = table_2way3_combined[, 1:3]

table_2way3_combined %>%
  kbl(format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

# Cotrimoxazole

table_2way1 = table(groupAstats$Group, groupAstats$Antibiotic)
table_2way2 = table(groupBstats$Group, groupBstats$Antibiotic)

# Convert the table to a data frame for better handling
table_2way4_df = as.data.frame(rbind(table_2way1, table_2way2))
table_2way4_df$Total = apply(table_2way4_df, 1, sum)

# Calculate row percentages
table_2way4_percent = table_2way4_df %>%
  mutate(No_Percent = No / Total * 100,
         Yes_Percent = Yes / Total * 100)

# Display counts with percentages in a combined format

table_2way4_combined = table_2way4_percent %>%
  mutate(No = paste0(as.character(No), " (", round(No_Percent, 1), "%)"),
         Yes = paste0(as.character(Yes), " (", round(Yes_Percent, 1), "%)"),
         Total = paste0(as.character(Total), " (100%)"))

table_2way4_combined = table_2way4_combined[, 1:3]

table_2way4_combined %>%
  kbl(format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

# Feeding & Cotrimoxazole (FA)

table_2way1 = table(groupAstats$Group, groupAstats$FA)
table_2way2 = table(groupBstats$Group, groupBstats$FA)

# Convert the table to a data frame for better handling
table_2way5_df = as.data.frame(rbind(table_2way1, table_2way2))
table_2way5_df$Total = apply(table_2way5_df, 1, sum)

# Calculate row percentages
table_2way5_percent = table_2way5_df %>%
  mutate(FA0_Percent = `0` / Total * 100,
         FA1_Percent = `1` / Total * 100,
         FA2_Percent = `2` / Total * 100,
         FA3_Percent = `3` / Total * 100)

# Display counts with percentages in a combined format
table_2way5_combined = table_2way5_percent %>%
  mutate(`0` = paste0(as.character(`0`), " (", round(FA0_Percent, 1), "%)"),
         `1` = paste0(as.character(`1`), " (", round(FA1_Percent, 1), "%)"),
         `2` = paste0(as.character(`2`), " (", round(FA2_Percent, 1), "%)"),
         `3` = paste0(as.character(`3`), " (", round(FA3_Percent, 1), "%)"),
         Total = paste0(as.character(Total), " (100%)"))

table_2way5_combined = t(table_2way5_combined[, 1:5])

table_2way5_combined %>%
  kbl(format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

# Sex

table_2way1 = table(groupAstats$Group, groupAstats$Gender)
table_2way2 = table(groupBstats$Group, groupBstats$Gender)

# Convert the table to a data frame for better handling
table_2way6_df = as.data.frame(rbind(table_2way1, table_2way2))
table_2way6_df$Total = apply(table_2way6_df, 1, sum)

# Calculate row percentages
table_2way6_percent = table_2way6_df %>%
  mutate(F_Percent = `F` / Total * 100,
         M_Percent = M / Total * 100)

# Display counts with percentages in a combined format
table_2way6_combined = table_2way6_percent %>%
  mutate(`F` = paste0(as.character(`F`), " (", round(F_Percent, 1), "%)"),
         M = paste0(as.character(M), " (", round(M_Percent, 1), "%)"),
         Total = paste0(as.character(Total), " (100%)"))

table_2way6_combined = table_2way6_combined[, 1:3]

table_2way6_combined %>%
  kbl(format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

# Sex & MVA85A

table_2way1 = table(groupAstats$Gender, groupAstats$Treatment)
table_2way2 = table(groupBstats$Gender, groupBstats$Treatment)
table_2wayplus = table_2way1 + table_2way2

# Convert the table to a data frame for better handling
table_2way7_df = as.data.frame(table_2wayplus)
table_2way7_df = dcast(table_2way7_df, Var1 ~ Var2, value.var = "Freq")
row.names(table_2way7_df) = levels(table_2way7_df$Var1)
table_2way7_df = table_2way7_df[, -1]
table_2way7_df$Total = apply(table_2way7_df, 1, sum)

# Calculate row percentages
table_2way7_percent = table_2way7_df %>%
  mutate(Control_Percent = Control / (Control + MVA85A) * 100,
         MVA85A_Percent = MVA85A / (Control + MVA85A) * 100)


# Display counts with percentages in a combined format
table_2way7_combined = table_2way7_percent %>%
  mutate(Control = paste0(Control, " (", round(Control_Percent, 1), "%)"),
         MVA85A = paste0(MVA85A, " (", round(MVA85A_Percent, 1), "%)"),
         Total = paste0(Total, " (100%)"))

table_2way7_combined = table_2way7_combined[, 1:3]

table_2way7_combined %>%
  kbl(format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

# Sex & QFT

table_2way1 = table(groupAstats$Gender, groupAstats$QFT)
table_2way2 = table(groupBstats$Gender, groupBstats$QFT)
table_2wayplus = table_2way1 + table_2way2

# Convert the table to a data frame for better handling
table_2way8_df = as.data.frame(table_2wayplus)
table_2way8_df = dcast(table_2way8_df, Var1 ~ Var2, value.var = "Freq")
row.names(table_2way8_df) = levels(table_2way8_df$Var1)
table_2way8_df = table_2way8_df[, -1]
table_2way8_df$Total = apply(table_2way8_df, 1, sum)

# Calculate row percentages
table_2way8_percent = table_2way8_df %>%
  mutate(Negative_Percent = Negative / (Negative + Positive) * 100,
         Positive_Percent = Positive / (Negative + Positive) * 100)

# Display counts with percentages in a combined format
table_2way8_combined = table_2way8_percent %>%
  mutate(Negative = paste0(Negative, " (", round(Negative_Percent, 1), "%)"),
         Positive = paste0(Positive, " (", round(Positive_Percent, 1), "%)"),
         Total = paste0(Total, " (100%)"))

table_2way8_combined = table_2way8_combined[, 1:3]

table_2way8_combined %>%
  kbl(format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

# Sex & FA

table_2way1 = table(groupAstats$Gender, groupAstats$FA)
table_2way2 = table(groupBstats$Gender, groupBstats$FA)
table_2wayplus = table_2way1 + table_2way2

# Convert the table to a data frame for better handling
table_2way9_df = as.data.frame(table_2wayplus)
table_2way9_df = dcast(table_2way9_df, Var1 ~ Var2, value.var = "Freq")
row.names(table_2way9_df) = c("F", "M")
table_2way9_df = table_2way9_df[, -1]
table_2way9_df$Total = apply(table_2way9_df, 1, sum)


# Calculate row percentages
table_2way9_percent = table_2way9_df %>%
  mutate(FA0_Percent = `0` / (`0` + `1` + `2` + `3`) * 100,
         FA1_Percent = `1` / (`0` + `1` + `2` + `3`) * 100,
         FA2_Percent = `2` / (`0` + `1` + `2` + `3`) * 100,
         FA3_Percent = `3` / (`0` + `1` + `2` + `3`) * 100)

# Display counts with percentages in a combined format
table_2way9_combined = table_2way9_percent %>%
  mutate(`0` = paste0(`0`, " (", round(FA0_Percent, 1), "%)"),
         `1` = paste0(`1`, " (", round(FA1_Percent, 1), "%)"),
         `2` = paste0(`2`, " (", round(FA2_Percent, 1), "%)"),
         `3` = paste0(`3`, " (", round(FA3_Percent, 1), "%)"),
         Total = paste0(Total, " (100%)"))

table_2way9_combined = t(table_2way9_combined[, 1:5])

table_2way9_combined %>%
  kbl(format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

# Fisher's Exact Tests for dependence between feeding and cotrimoxazole

# Nonparametric test

fant_A = table(groupA$Feeding, groupA$Antibiotic)

fisher.test(x = fant_A)

fant_B = table(groupB$Feeding, groupB$Antibiotic)

fisher.test(x = fant_B)

# Evidence of dependence in Group A but not Group B - supports combined VOI (FA)

# Change labels for plotting

varA[8] = "%CD8+ ANYcytok+"
varA[14:21] = c("Bulk CD4+ %R7-RA+", "Bulk CD4+ %R7+RA+", "Bulk CD4+ %R7+RA-", "Bulk CD4+ %R7-RA-",
                "Bulk CD8+ %R7-RA+", "Bulk CD8+ %R7+RA+", "Bulk CD8+ %R7+RA-", "Bulk CD8+ %R7-RA-")

varA = varA[-c(1, 10, 11, 12, 28, 33, 38)]

varB[5:16] =  c("%CD16+ PMN", "%CD14+ Mono", "%CD19+ B", "%CD3+ T", "%CD56+ NKTlike", "%CD8+ T",     
                "%HLA-DR+ CD8+ T", "%CD4+ T", "%HLA-DR+ CD4+ T", "%gd TCR+",  "%HLA-DR+ gd T",  "%totNK")
varB = varB[-(1:7)]

# Separate data by outcome; removing GrL+ responses and infrequently observed outcomes

outA = groupA[, -c(1:17, 26, 27, 28, 44, 49, 54)]
colnames(outA) = varA
outB = groupB[, c(23:34)]
colnames(outB) = varB

# Correlations

# Group A

corA = cbind('Time' = groupA$`Time`, outA)

corA_visit1 = corA[corA$Time == 56, ]
corA_visit1 = corA_visit1[complete.cases(corA_visit1), ]
corA_visit1 = as.matrix(corA_visit1[, -c(1,35)])

corA1mat = cor(corA_visit1, method = "spearman")
corA1mat = melt(corA1mat)

corA_visit2 = corA[corA$Time == 112, ]
corA_visit2 = corA_visit2[complete.cases(corA_visit2), ]
corA_visit2 = as.matrix(corA_visit2[, -c(1, 35)])

corA2mat = cor(corA_visit2, method = "spearman")
corA2mat = melt(corA2mat)

corA_visit3 = corA[corA$Time == 365, ]
corA_visit3 = corA_visit3[complete.cases(corA_visit3), ]
corA_visit3 = as.matrix(corA_visit3[, -c(1, 35)])

corA3mat = cor(corA_visit3, method = "spearman")
corA3mat = melt(corA3mat)

# Heatmap of all Group A outcomes on Days

ggplot(corA1mat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "black") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7, angle = 90, family = "Arial"),
        axis.text.y = element_text(size = 7, family = "Arial"),
        legend.text = element_text(size = 7, family = "Arial"),
        legend.title = element_blank()) +
  scale_fill_gradient2(low = "#075AFF",
                       mid = "white",
                       high = "#FF0000") + coord_fixed()

ggplot(corA2mat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "black") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7, angle = 90, family = "Arial"),
        axis.text.y = element_text(size = 7, family = "Arial"),
        legend.text = element_text(size = 7, family = "Arial"),
        legend.title = element_blank()) +
  scale_fill_gradient2(low = "#075AFF",
                       mid = "white",
                       high = "#FF0000") + coord_fixed()

ggplot(corA3mat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "black") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7, angle = 90, family = "Arial"),
        axis.text.y = element_text(size = 7, family = "Arial"),
        legend.text = element_text(size = 7, family = "Arial"),
        legend.title = element_blank()) +
  scale_fill_gradient2(low = "#075AFF",
                       mid = "white",
                       high = "#FF0000") + coord_fixed()

# Spaghetti plots of Group A outcomes

ggplot(data = groupA, aes(x = factor(Time, levels = c("56", "112", "365")), y = O24, group = ID)) + 
  geom_line() + stat_summary(aes(group = 1), fun.y = mean, geom = "line", shape = 17, size = 1, color = "#075AFF") +
  xlab("\n Time") + ylab(paste0(varA[24], "\n")) + theme_bw() + 
  theme(axis.title.x = element_text(size = 12, family = "Arial", face = "bold"),
        axis.title.y = element_text(size = 12, family = "Arial", face = "bold"),
        axis.text.x = element_text(size = 10, family = "Arial"),
        axis.text.y = element_text(size = 10, family = "Arial"),
        legend.text = element_text(size = 10, family = "Arial")) + facet_wrap(~`Treatment`)


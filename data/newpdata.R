library(tidyverse)
library(writexl)
library(readxl)
library(ggplot2)
library(ggpubr)
library(ggh4x)
library(limma) 
library(survival)
library(survminer)
library(googledrive)
library(dplyr)
library(zscorer)
library(tableone)
library(EnhancedVolcano)
library(ComplexHeatmap)
library(miscTools)
library(circlize)
library(png)
library(cowplot)
library(tmod)  
library(Biobase)
library(tidyverse)
library(EDASeq)
library(edgeR)
library(ComplexHeatmap)
library(variancePartition)
library(parallel)

temp<- tempfile(fileext = ".rds")
dl <- drive_download(
  as_id("1CVfiloFh5iqwaO4geS49FMDLdvK-jLdm"), path = temp, overwrite = TRUE)
z <- readRDS(file = dl$local_path)

dim(pData(z))

table(pData(z)$Cohort.Number)

pdata <- pData(z)

pdata <- pdata %>%
  dplyr::select(
    -Raw.PfCSP.ELISA.OD.preimmune,
    -Raw.PfCSP.ELISA.OD.two.weeks.post.VAX3,
    -Net.PfCSP.ELISA.OD.two.weeks.post.VAX3...minus.preimmune.,
    -Ratio.PfCSP.ELIS A.OD.ratio.two.weeks.post.VAX3..by.preimmune.,
    -Raw.PfCSP.ELISA.OD.preCHMI,
    -Net.PfCSP.ELISA.OD.preCHMI..minus.preimmune.,
    -Ratio.PfCSP.ELISA.OD.pre.CHMI..by.preimmune.,
    -Raw.PfCSP.ELISA.OD.CHMI,
    -Net.PfCSP.ELISA.OD.CHMI..minus.preimmune.,
    -Ratio.PfCSP.ELISA.OD.CHMI..by.preimmune.,
    -Raw.aIFA..AFU.2x105..preimmune,
    -Raw.aIFA..AFU.2x105..two.weeks.post.VAX3,
    -Net.aIFA..AFU.2x105..two.weeks.post.VAX3...minus.preimmune.,
    -Ratio.aIFA..AFU.2x105..ratio.two.weeks.post.VAX3..by.preimmune.,
    -Raw.aIFA..AFU.2x105..preCHMI,
    -Net.aIFA..AFU.2x105..preCHMI..minus.preimmune.,
    -Ratio.aIFA..AFU.2x105..pre.CHMI..by.preimmune.,
    -Raw.aIFA..AFU.2x105..CHMI,
    -Net.aIFA..AFU.2x105..CHMI..minus.preimmune.,
    -Ratio.aIFA..AFU.2x105..CHMI..by.preimmune.,
    -Raw..ISI..Reciprocal.serum.dilution.for.80..inhibition..preimmune,
    -Raw..ISI..Reciprocal.serum.dilution.for.80..inhibition..two.weeks.post.VAX3,
    -Net..ISI..Reciprocal.serum.dilution.for.80..inhibition..two.weeks.post.VAX3...minus.preimmune.,
    -Ratio..ISI..Reciprocal.serum.dilution.for.80..inhibition..ratio.two.weeks.post.VAX3..by.preimmune.,
    -Raw..ISI..Reciprocal.serum.dilution.for.80..inhibition..preCHMI,
    -Net..ISI..Reciprocal.serum.dilution.for.80..inhibition..preCHMI..minus.preimmune.,
    -Ratio..ISI..Reciprocal.serum.dilution.for.80..inhibition..pre.CHMI..by.preimmune.,
    -Raw..ISI..Reciprocal.serum.dilution.for.80..inhibition..CHMI,
    -Net..ISI..Reciprocal.serum.dilution.for.80..inhibition..CHMI..minus.preimmune.,
    -Visit.No.,
    -Timing,
    -Timepoint_original,
    -Ratio..ISI..Reciprocal.serum.dilution.for.80..inhibition..CHMI..by.preimmune.
  )
pData(z) <- pdata
dim(pData(z))
colnames(pData(z))
z <- z[, pData(z)$Cohort.Number != "Cohort_2"]
table(pData(z)$Cohort.Number)
dim(pData(z))
z <- z[, is.na(pData(z)$Notes)]
table(pData(z)$Cohort.Number)
table(pData(z)$Cohort.Number)
dim(pData(z))
pData(z) <- pData(z)[ , colnames(pData(z)) != "Notes"]
dim(pData(z))
colnames(pData(z))
unique(pData(z)$timepoint_wrtVAX)
table(pData(z)$Cohort.Number, pData(z)$timepoint_wrtVAX)
z <- z[, !pData(z)$timepoint_wrtVAX %in% c("CHMI_D1", "VAX2", "VAX3")]
dim(pData(z))
pData(z)$antibody_response <- NA
colnames(pData(z))
high_ids <- c(
  "M18GHC065","M18GHC066","M18GHC068","M18GHC073",
  "M18GHC026","M18GHC019","M18GHC002","M18GHC007",
  "M18GHC008","M18GHC009"
)

pData(z)$antibody_response[pData(z)$Subject.ID %in% high_ids] <- "High"
pData(z)$antibody_response[is.na(pData(z)$antibody_response)] <- "Low"
mapping <- data.frame(
  Old_Subject_ID = c(
    "M18GHC001","M18GHC002","M18GHC007","M18GHC008","M18GHC009",
    "M18GHC012","M18GHC015","M18GHC019","M18GHC026","M18GHC047",
    "M18GHC065","M18GHC066","M18GHC068","M18GHC069","M18GHC073"
  ),
  Anon_Subject_ID = c(
    "SUBJ_0001","SUBJ_0002","SUBJ_0003","SUBJ_0004","SUBJ_0005",
    "SUBJ_0006","SUBJ_0007","SUBJ_0008","SUBJ_0009","SUBJ_0010",
    "SUBJ_0011","SUBJ_0012","SUBJ_0013","SUBJ_0014","SUBJ_0015"
  ),
  stringsAsFactors = FALSE
)
pData(z)$Subject.ID <- mapping$Anon_Subject_ID[
  match(pData(z)$Subject.ID, mapping$Old_Subject_ID)
]
unique(pData(z)$Subject.ID)
head(pData(z)[, c("Subject.ID")])
pdata <- pData(z)
saveRDS(pdata, file = "phenodata_clean.rds")
getwd()
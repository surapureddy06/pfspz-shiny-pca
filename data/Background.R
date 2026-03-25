
library(here)
library(Biobase)
library(edgeR)
library(ggplot2)
library(dplyr)


pdata <- readRDS(here("data", "phenodata_clean.rds"))
z <- readRDS(here("data", "z_matched_clean.rds"))

class(pdata)
class(z)

dim(pdata)
dim(exprs(z))
dim(pData(z))

head(pdata)
head(pData(z))
head(sampleNames(z))

all(sampleNames(z) == pData(z)$filenames)

expr_matrix <- exprs(z)

dim(expr_matrix)
expr_matrix[1:5, 1:5]

saveRDS(expr_matrix, here("data", "expression_matrix.rds"))

dge <- DGEList(
  counts = expr_matrix,
  samples = pData(z)
)

dge

saveRDS(dge, here("data", "dge_list.rds"))

keep <- filterByExpr(dge)

dge <- dge[keep, , keep.lib.sizes = FALSE]
dim(dge)
saveRDS(dge, here("data", "dge_filtered.rds"))
saveRDS(dge$counts, here("data", "filtered_counts.rds"))

dge <- calcNormFactors(dge)
dge$samples

log_cpm <- cpm(dge, log = TRUE)
dim(log_cpm)
head(log_cpm)
saveRDS(log_cpm, here("data", "logCPM_normalized.rds"))

pca <- prcomp(t(log_cpm), scale. = TRUE)

pca_df <- as.data.frame(pca$x)


pca_df$Sex <- pData(z)$Sex
pca_df$Outcome <- pData(z)$Outcome
pca_df$Antibody <- pData(z)$antibody_response


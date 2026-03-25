# =========================
# Load libraries
# =========================
library(here)
library(Biobase)
library(edgeR)
library(ggplot2)
library(dplyr)
library(viridis)

# =========================
# Step 1: Load data
# =========================
pdata <- readRDS(here("data", "phenodata_clean.rds"))
z <- readRDS(here("data", "z_matched_clean.rds"))

# =========================
# Step 2: Quick checks
# =========================
print(class(pdata))
print(class(z))

print(dim(pdata))
print(dim(counts(z)))
print(dim(pData(z)))

print(head(pdata))
print(head(pData(z)))
print(head(sampleNames(z)))

# Check alignment
if ("filenames" %in% colnames(pData(z))) {
  print(all(sampleNames(z) == pData(z)$filenames))
}

# =========================
# Step 3: Extract expression matrix
# =========================
expr_matrix <- counts(z)

print(dim(expr_matrix))
print(expr_matrix[1:5, 1:5])

saveRDS(expr_matrix, here("data", "expression_matrix.rds"))

# =========================
# Step 4: Create DGEList
# =========================
dge <- DGEList(
  counts = expr_matrix,
  samples = pData(z)
)

saveRDS(dge, here("data", "dge_list.rds"))

# =========================
# Step 5: Filter genes
# =========================
keep <- filterByExpr(dge)
dge <- dge[keep, , keep.lib.sizes = FALSE]

saveRDS(dge, here("data", "dge_filtered.rds"))

# =========================
# Step 6: Normalize
# =========================
dge <- calcNormFactors(dge)

# =========================
# Step 7: logCPM
# =========================
log_cpm <- cpm(dge, log = TRUE, prior.count = 1)

saveRDS(log_cpm, here("data", "logCPM_normalized.rds"))

# =========================
# Step 8: Baseline subset
# =========================
baseline_idx <- pData(z)$timepoint_wrtVAX == "VAX1"
z_baseline <- z[, baseline_idx]

# =========================
# Step 9: Baseline DGE
# =========================
baseline_expr <- counts(z_baseline)

dge_baseline <- DGEList(
  counts = baseline_expr,
  samples = pData(z_baseline)
)

keep_baseline <- filterByExpr(dge_baseline)
dge_baseline <- dge_baseline[keep_baseline, , keep.lib.sizes = FALSE]

dge_baseline <- calcNormFactors(dge_baseline)

log_cpm_baseline <- cpm(dge_baseline, log = TRUE, prior.count = 1)

# =========================
# Step 10: PCA
# =========================
pca_baseline <- prcomp(t(log_cpm_baseline), scale. = TRUE)

percent_var <- (pca_baseline$sdev^2 / sum(pca_baseline$sdev^2)) * 100

# =========================
# Step 11: PCA dataframe
# =========================
meta_baseline <- pData(z_baseline)

pca_baseline_df <- as.data.frame(pca_baseline$x[, 1:5]) %>%
  mutate(
    Sex = as.character(meta_baseline$Sex),
    Outcome = as.character(meta_baseline$Outcome),
    Antibody = as.character(meta_baseline$antibody_response),
    SubjectID = as.factor(meta_baseline$Subject.ID),
    ParasitemiaDays = as.numeric(meta_baseline$tte.parasitemia.days)
  ) %>%
  mutate(
    ParasitemiaDays = ifelse(is.na(ParasitemiaDays), 0, ParasitemiaDays)
  )

# =========================
# Step 12: Clean labels
# =========================
pca_baseline_df <- pca_baseline_df %>%
  mutate(
    Sex = ifelse(Sex %in% c("M", "Male", "male"), "Male", "Female"),
    Outcome = ifelse(
      grepl("not|non|infect", Outcome, ignore.case = TRUE),
      "Non-Protected",
      "Protected"
    ),
    Antibody = ifelse(
      grepl("high", Antibody, ignore.case = TRUE),
      "High",
      "Low"
    )
  )

# =========================
# Step 13: Theme + labels
# =========================
nice_theme <- theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA)
  )

x_lab <- paste0("PC1 (", round(percent_var[1], 1), "%)")
y_lab <- paste0("PC2 (", round(percent_var[2], 1), "%)")

# =========================
# Step 14: Outcome
# =========================
p1 <- ggplot(pca_baseline_df, aes(PC1, PC2, color = Outcome)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Protected" = "green", "Non-Protected" = "red")) +
  labs(title = "Baseline PCA", subtitle = "Outcome", x = x_lab, y = y_lab) +
  nice_theme
print(p1)

# =========================
# Step 15: Antibody
# =========================
p2 <- ggplot(pca_baseline_df, aes(PC1, PC2, color = Antibody)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("High" = "blue", "Low" = "gold")) +
  labs(title = "Baseline PCA", subtitle = "Antibody", x = x_lab, y = y_lab) +
  nice_theme
print(p2)

# =========================
# Step 16: Sex
# =========================
p3 <- ggplot(pca_baseline_df, aes(PC1, PC2, color = Sex)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "pink")) +
  labs(title = "Baseline PCA", subtitle = "Sex", x = x_lab, y = y_lab) +
  nice_theme
print(p3)

# =========================
# Step 17: Parasitemia
# =========================
p4 <- ggplot(pca_baseline_df, aes(PC1, PC2, color = ParasitemiaDays)) +
  geom_point(size = 4) +
  scale_color_viridis_c(option = "B", name = "Parasitemia Days") +
  labs(title = "Baseline PCA", subtitle = "Parasitemia", x = x_lab, y = y_lab) +
  nice_theme
print(p4)

# =========================
# Step 18: SubjectID (more colorful)
# =========================
p5 <- ggplot(pca_baseline_df, aes(PC1, PC2, color = SubjectID)) +
  geom_point(size = 4, alpha = 0.9) +
  scale_color_viridis_d(option = "turbo") +
  labs(title = "Baseline PCA", subtitle = "Subject ID", x = x_lab, y = y_lab) +
  nice_theme
print(p5)

# =========================
# Step 19: Save plots
# =========================
ggsave(here("data", "Baseline_PCA_Outcome.png"), p1)
ggsave(here("data", "Baseline_PCA_Antibody.png"), p2)
ggsave(here("data", "Baseline_PCA_Sex.png"), p3)
ggsave(here("data", "Baseline_PCA_Parasitemia.png"), p4)
ggsave(here("data", "Baseline_PCA_SubjectID.png"), p5)

# =========================
# Step 20: Save objects
# =========================
saveRDS(pca_baseline, here("data", "pca_baseline.rds"))
saveRDS(pca_baseline_df, here("data", "pca_baseline_df.rds"))

cat("Baseline PCA DONE\n")
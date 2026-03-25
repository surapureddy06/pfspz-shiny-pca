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
# Step 2: Remove baseline (VAX1)
# =========================
post_idx <- pData(z)$timepoint_wrtVAX != "VAX1"
z_post <- z[, post_idx]

# =========================
# Step 3: Extract expression
# =========================
post_expr <- counts(z_post)

# =========================
# Step 4: Create DGEList
# =========================
dge_post <- DGEList(
  counts = post_expr,
  samples = pData(z_post)
)

# =========================
# Step 5: Filter genes
# =========================
keep_post <- filterByExpr(
  dge_post,
  group = pData(z_post)$timepoint_wrtVAX
)

dge_post <- dge_post[keep_post, , keep.lib.sizes = FALSE]

# =========================
# Step 6: Normalize
# =========================
dge_post <- calcNormFactors(dge_post)

# =========================
# Step 7: logCPM
# =========================
log_cpm_post <- cpm(dge_post, log = TRUE, prior.count = 1)

# =========================
# Step 8: PCA
# =========================
pca_post <- prcomp(t(log_cpm_post), scale. = TRUE)

percent_var_post <- (pca_post$sdev^2 / sum(pca_post$sdev^2)) * 100

# =========================
# Step 9: PCA dataframe
# =========================
pca_post_df <- as.data.frame(pca_post$x[, 1:5])

meta <- pData(z_post)

pca_post_df <- pca_post_df %>%
  mutate(
    Sex = as.character(meta$Sex),
    Outcome = as.character(meta$Outcome),
    Antibody = as.character(meta$antibody_response),
    Timepoint = as.character(meta$timepoint_wrtVAX),
    SubjectID = as.factor(meta$Subject.ID),
    ParasitemiaDays = as.numeric(meta$tte.parasitemia.days)
  ) %>%
  mutate(
    ParasitemiaDays = ifelse(is.na(ParasitemiaDays), 0, ParasitemiaDays)
  )

# =========================
# Step 10: Clean labels
# =========================
pca_post_df <- pca_post_df %>%
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
# Step 11: Colors
# =========================
sex_colors <- c("Male" = "blue", "Female" = "pink")
outcome_colors <- c("Protected" = "green", "Non-Protected" = "red")
antibody_colors <- c("High" = "blue", "Low" = "gold")

# =========================
# Step 12: Theme
# =========================
nice_pca_theme <- theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA),
    plot.background = element_rect(fill = "white"),
    legend.position = "right"
  )

x_lab <- paste0("PC1 (", round(percent_var_post[1], 1), "%)")
y_lab <- paste0("PC2 (", round(percent_var_post[2], 1), "%)")

# =========================
# Step 13: Outcome
# =========================
p1 <- ggplot(pca_post_df, aes(PC1, PC2, color = Outcome)) +
  geom_point(size = 4) +
  scale_color_manual(values = outcome_colors) +
  labs(
    title = "Post-Vaccination PCA",
    subtitle = "Outcome",
    x = x_lab,
    y = y_lab
  ) +
  nice_pca_theme
print(p1)

# =========================
# Step 14: Antibody
# =========================
p2 <- ggplot(pca_post_df, aes(PC1, PC2, color = Antibody)) +
  geom_point(size = 4) +
  scale_color_manual(values = antibody_colors) +
  labs(
    title = "Post-Vaccination PCA",
    subtitle = "Antibody",
    x = x_lab,
    y = y_lab
  ) +
  nice_pca_theme
print(p2)

# =========================
# Step 15: Sex
# =========================
p3 <- ggplot(pca_post_df, aes(PC1, PC2, color = Sex)) +
  geom_point(size = 4) +
  scale_color_manual(values = sex_colors) +
  labs(
    title = "Post-Vaccination PCA",
    subtitle = "Sex",
    x = x_lab,
    y = y_lab
  ) +
  nice_pca_theme
print(p3)

# =========================
# Step 16: Timepoint
# =========================
p4 <- ggplot(pca_post_df, aes(PC1, PC2, color = Timepoint)) +
  geom_point(size = 4) +
  labs(
    title = "Post-Vaccination PCA",
    subtitle = "Timepoint",
    x = x_lab,
    y = y_lab
  ) +
  nice_pca_theme
print(p4)

# =========================
# Step 17: Parasitemia
# =========================
p5 <- ggplot(pca_post_df, aes(PC1, PC2, color = ParasitemiaDays)) +
  geom_point(size = 4) +
  scale_color_viridis_c(
    option = "B",
    direction = 1,
    name = "Parasitemia Days"
  ) +
  labs(
    title = "Post-Vaccination PCA",
    subtitle = "Parasitemia",
    x = x_lab,
    y = y_lab
  ) +
  nice_pca_theme
print(p5)

# =========================
# Step 18: SubjectID (more colorful)
# =========================
p6 <- ggplot(pca_post_df, aes(PC1, PC2, color = SubjectID)) +
  geom_point(size = 4, alpha = 0.9) +
  scale_color_viridis_d(option = "turbo") +
  labs(
    title = "Post-Vaccination PCA",
    subtitle = "Subject ID",
    x = x_lab,
    y = y_lab
  ) +
  nice_pca_theme
print(p6)

# =========================
# Step 19: Save plots
# =========================
ggsave(here("data", "PCA_by_Outcome.png"), p1, width = 8, height = 6)
ggsave(here("data", "PCA_by_Antibody.png"), p2, width = 8, height = 6)
ggsave(here("data", "PCA_by_Sex.png"), p3, width = 8, height = 6)
ggsave(here("data", "PCA_by_Timepoint.png"), p4, width = 8, height = 6)
ggsave(here("data", "PCA_by_ParasitemiaDays.png"), p5, width = 8, height = 6)
ggsave(here("data", "PCA_by_SubjectID.png"), p6, width = 8, height = 6)

# =========================
# Step 20: Save objects
# =========================
saveRDS(pca_post, here("data", "pca_postVAX.rds"))
saveRDS(pca_post_df, here("data", "pca_postVAX_df.rds"))

cat("PCA DONE\n")
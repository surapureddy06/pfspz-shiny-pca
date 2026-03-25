# =========================
# Load libraries
# =========================
library(Biobase)
library(googledrive)
library(here)

# =========================
# Step 1: Load cleaned phenodata
# =========================
pdata <- readRDS(here("data", "phenodata_clean.rds"))

# Check phenodata
dim(pdata)
colnames(pdata)
head(pdata$filenames)

# =========================
# Step 2: Load original z object from Google Drive
# =========================
temp <- tempfile(fileext = ".rds")

dl <- drive_download(
  as_id("1CVfiloFh5iqwaO4geS49FMDLdvK-jLdm"),
  path = temp,
  overwrite = TRUE
)

z <- readRDS(file = dl$local_path)

# Check z
dim(exprs(z))
dim(pData(z))
head(sampleNames(z))

# =========================
# Step 3: Keep only filenames present in cleaned phenodata
# =========================
keep_files <- as.character(pdata$filenames)

# Check which filenames are common
common_files <- intersect(keep_files, sampleNames(z))

length(common_files)
head(common_files)

# =========================
# Step 4: Subset z to only matched samples
# =========================
z_matched <- z[, sampleNames(z) %in% common_files]

# Reorder z_matched to match phenodata order
z_matched <- z_matched[, match(common_files, sampleNames(z_matched))]

# Reorder pdata to match z_matched
pdata_matched <- pdata[match(sampleNames(z_matched), pdata$filenames), ]

# =========================
# Step 5: Replace pData in z_matched with cleaned matched phenodata
# =========================

pData(z_matched) <- pdata_matched

saveRDS(z_matched, file = "z_matched_clean.rds")

getwd()


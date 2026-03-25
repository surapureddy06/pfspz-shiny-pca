install.packages(c("readxl", "dplyr", "tibble", "writexl", "here"))
install.packages("lifecycle")
install.packages("dplyr")
library(readxl)
library(dplyr)
library(here)

library(readxl)
library(here)

file_path <- here("data", "U01_Human PfSPZ CVac VTEU-0042 trial pheno data.xlsx")
df <- read_excel(file_path)
names(df)
head(df)

"Subject.ID" %in% names(df)
missing_id_count <- sum(is.na(df$Subject.ID) | df$Subject.ID == "")
missing_id_count
id_map <- df %>%
  distinct(Subject.ID) %>%                 # keep only unique Subject IDs
  filter(!(is.na(Subject.ID) | Subject.ID == "")) %>%  # exclude blanks from mapping
  arrange(Subject.ID) %>%                  # stable ordering
  mutate(Anon_Subject_ID = sprintf("SUBJ_%04d", row_number())) %>%
  rename(Old_Subject_ID = Subject.ID)
head(id_map, 10)
stopifnot(nrow(id_map) == n_distinct(id_map$Old_Subject_ID))
df_anon <- df %>%
  left_join(id_map, by = c("Subject.ID" = "Old_Subject_ID")) %>%  # attach anonymous ID
  mutate(
    Subject.ID = ifelse(
      is.na(Anon_Subject_ID),
      Subject.ID,               # keep as-is if original was blank/NA
      Anon_Subject_ID           # replace with anonymized ID
    )
  ) %>%
  select(-Anon_Subject_ID)      # remove helper column
nrow(df)
nrow(df_anon)
stopifnot(nrow(df_anon) == nrow(df))
head(df_anon$Subject.ID, 20)
old_unique <- df %>% filter(!(is.na(Subject.ID) | Subject.ID == "")) %>% summarise(n = n_distinct(Subject.ID)) %>% pull(n)
new_unique <- df_anon %>% filter(!(is.na(Subject.ID) | Subject.ID == "")) %>% summarise(n = n_distinct(Subject.ID)) %>% pull(n)

old_unique
new_unique
stopifnot(old_unique == new_unique)
anon_path <- here("data", "U01_VTEU0042_pheno_ANON.xlsx")
write_xlsx(df_anon, anon_path)
anon_path

map_path <- here("data", "U01_VTEU0042_SubjectID_MAPPING_PRIVATE.xlsx")
write_xlsx(id_map, map_path)
map_path
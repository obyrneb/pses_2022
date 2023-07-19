## LOAD PSES 2022 DATA

## This script downloads the six different 2022/2023 Public Service Employee Survey
## data subsets and then combines them into a single dataframe. For convenience,
## we save this dataframe into an RDS file for later retrieval when calling
## this script. 

library(tidyverse)
library(janitor)

# Set directory names
data_dir <- file.path(getwd(), "data")
plot_dir <- file.path(getwd(), "plots")
output_dir <- file.path(getwd(), "output")

# Create directories if they don't yet exist
ifelse(!dir.exists(file.path(data_dir)), dir.create(file.path(data_dir)), FALSE)
ifelse(!dir.exists(file.path(plot_dir)), dir.create(file.path(plot_dir)), FALSE)
ifelse(!dir.exists(file.path(output_dir)), dir.create(file.path(output_dir)), FALSE)

# This function checks if a file exist and, if not, downloads it.
check_file <- function(file_path, file_url) {
  if (!file.exists(file_path)) {
    safely(download.file(file_url, file_path))
  }
}

# Build a dataframe with the subset names (ss1, ss2, etc.), URLs, file names and file paths.
pses_2022_files <- 
  tibble(
    name = paste0("ss", c("1","2","3","4","4h","5","6","7"), "_2022"),
    url = c(
      "https://open.canada.ca/data/dataset/9f9501f3-8188-4386-87ba-50730a979044/resource/526ea41d-d7cb-44a5-9982-8fc1d09221ac/download/subset-1-2022_2023-pses-open-dataset-with-labels_sous-ensemble-1-ensemble-de-donnees-ouvertes-du.csv",
      "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-2022-23/Subset%202%20-%202022_2023%20PSES%20open%20dataset%20-%20with%20labels_Sous-ensemble%202%20-%20Ensemble%20de%20donn%C3%A9es%20ouvertes%20du%20SAFF%20de%202022_23.csv",
      "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-2022-23/Subset%203%20-%202022_2023%20PSES%20open%20dataset%20-%20with%20labels_Sous-ensemble%203%20-%20Ensemble%20de%20donn%C3%A9es%20ouvertes%20du%20SAFF%20de%202022_23.csv",
      "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-2022-23/Subset%204%20-%202022_2023%20PSES%20open%20dataset%20-%20with%20labels_Sous-ensemble%204%20-%20Ensemble%20de%20donn%C3%A9es%20ouvertes%20du%20SAFF%20de%202022_23.csv",
      "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-2022-23/Subset%204%20-%202022_2023%20PSES%20open%20dataset%20historical%20-%20with%20labels_Sous-ensemble%204%20-%20Ensemble%20de%20donn%C3%A9es%20ouvertes%20historique%20du%20SAFF%20de%202022_23.csv",
      "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-2022-23/Subset%205%20-%202022_2023%20PSES%20open%20dataset%20-%20with%20labels_Sous-ensemble%205%20-%20Ensemble%20de%20donn%C3%A9es%20ouvertes%20du%20SAFF%20de%202022_23.csv",
      "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-2022-23/Subset%206%20-%202022_2023%20PSES%20open%20dataset%20-%20with%20labels_Sous-ensemble%206%20-%20Ensemble%20de%20donn%C3%A9es%20ouvertes%20du%20SAFF%20de%202022_23.csv",
      "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-2022-23/Subset%207%20-%202022_2023%20PSES%20open%20dataset%20-%20with%20labels_Sous-ensemble%207%20-%20Ensemble%20de%20donn%C3%A9es%20ouvertes%20du%20SAFF%20de%202022_23.csv"
    ) 
  ) %>% 
  mutate(
    file = basename(url) %>% URLdecode(),
    path = file.path(data_dir, file)
  )

# If we don't have an RDS file to read pses_2022 from, let's build it.
if(!file.exists("pses_2022.rds")) {
  
  # Check if each file exists. If not, download it.
  if(!file.exists(pses_2022_files$path[1])) {download.file(pses_2022_files$url[1], pses_2022_files$path[1])}
  if(!file.exists(pses_2022_files$path[2])) {download.file(pses_2022_files$url[2], pses_2022_files$path[2])}
  if(!file.exists(pses_2022_files$path[3])) {download.file(pses_2022_files$url[3], pses_2022_files$path[3])}
  if(!file.exists(pses_2022_files$path[4])) {download.file(pses_2022_files$url[4], pses_2022_files$path[4])}
  if(!file.exists(pses_2022_files$path[5])) {download.file(pses_2022_files$url[5], pses_2022_files$path[5])}
  if(!file.exists(pses_2022_files$path[6])) {download.file(pses_2022_files$url[6], pses_2022_files$path[6])}
  if(!file.exists(pses_2022_files$path[7])) {download.file(pses_2022_files$url[7], pses_2022_files$path[7])}
  if(!file.exists(pses_2022_files$path[8])) {download.file(pses_2022_files$url[8], pses_2022_files$path[8])}
  
  # Check if the pses_2022 dataframe exists. If not, read each subset and construct it.
  # We add a subset column and convert the names to snake case. 
  if (!exists("pses_2022")) {
    
    ss1_2022 <- read_csv(pses_2022_files$path[1], col_types = rep("c", 35), na = c("9999","X",""), locale = locale(encoding = "latin1")) %>% add_column(subset = "ss1") %>% clean_names()
    ss2_2022 <- read_csv(pses_2022_files$path[2], col_types = rep("c", 35), na = c("9999","X",""), locale = locale(encoding = "latin1")) %>% add_column(subset = "ss2") %>% clean_names()
    ss3_2022 <- read_csv(pses_2022_files$path[3], col_types = rep("c", 35), na = c("9999","X",""), locale = locale(encoding = "latin1")) %>% add_column(subset = "ss3") %>% clean_names()
    ss4_2022 <- read_csv(pses_2022_files$path[4], col_types = rep("c", 35), na = c("9999","X",""), locale = locale(encoding = "latin1")) %>% add_column(subset = "ss4") %>% clean_names()
    ss4h_2022 <- read_csv(pses_2022_files$path[5], col_types = rep("c", 35), na = c("9999","X",""), locale = locale(encoding = "latin1")) %>% add_column(subset = "ss4h") %>% clean_names()
    ss5_2022 <- read_csv(pses_2022_files$path[6], col_types = rep("c", 35), na = c("9999","X",""), locale = locale(encoding = "latin1")) %>% add_column(subset = "ss5") %>% clean_names()
    ss6_2022 <- read_csv(pses_2022_files$path[7], col_types = rep("c", 35), na = c("9999","X",""), locale = locale(encoding = "latin1")) %>% add_column(subset = "ss6") %>% clean_names()
    ss7_2022 <- read_csv(pses_2022_files$path[8], col_types = rep("c", 35), na = c("9999","X",""), locale = locale(encoding = "latin1")) %>% add_column(subset = "ss7") %>% clean_names()

    pses_2022 <- 
      # Bind all subsets together
      bind_rows(
        ss1_2022,
        ss2_2022,
        ss3_2022,
        ss4_2022,
        ss4h_2022,
        ss5_2022,
        ss6_2022,
        ss7_2022
      ) %>% 
      # Standardize language-specific column names with _e and _f, 
      # as opposed to ENG and FRA
      set_names(~sub("eng$","_e",.x)) %>%
      set_names(~sub("fra$","_f",.x)) %>% 
      # Create a "dem_question" column to group the "bycond" column by 
      # demographic question. Use "none" when NA and use "org" when
      # the "bycond" value refers to an organizational level. 
      # Otherwise, use the string preceding the "=" sign.
      left_join(
        distinct(., bycond) %>% 
          mutate(
            dem_question = case_when(
              is.na(bycond) ~ "none",
              startsWith(bycond, "LEVEL") ~ "org",
              TRUE ~ word(bycond, 1, sep = " =")
            )
          ),
        by = "bycond"
      ) %>% 
      # Rename the very wordy "most... and least..." columns 
      rename(
        positive = most_positive_or_least_negative,
        neutral = neutral_or_middle_category,
        negative = most_negative_or_least_positive
      )
    
  }
  
  # Save as an RDS file (so we don't need to do this again)
  saveRDS(pses_2022, file = "pses_2022.rds")
  
} else {
  
  # If we already have and RDS file, use that.
  pses_2022 <- readRDS(file = "pses_2022.rds")
  
}

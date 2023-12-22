library(scales)
library(readxl)
library(tidyverse)
library(ggrepel)
library(grid)
library(cowplot)
library(stringi)
library(janitor)



#----
# LOAD DATA

# Setting up our directories
data_dir <- file.path(getwd(), "data")
plot_dir <- file.path(getwd(), "plots")
output_dir <- file.path(getwd(), "output")

# Create our directories if they don't exist.
ifelse(!dir.exists(data_dir), dir.create(data_dir), FALSE)
ifelse(!dir.exists(plot_dir), dir.create(plot_dir), FALSE)
ifelse(!dir.exists(output_dir), dir.create(output_dir), FALSE)

# There report cards compare two years of PSES data. To avoid hardcoding those
# years, we will use the convention of "this_year" and "last_year".
# Set those years below:
this_y <- 2022
last_y <- 2020

# Setting up our data files
# For convenience, we are using the default names for these files.
last_year_file <- file.path(data_dir,"subset-7-sous-ensemble-7-1.csv")
this_year_file <- file.path(data_dir,"Subset 7 - 2022_2023 PSES open dataset - with labels_Sous-ensemble 7 - Ensemble de données ouvertes du SAFF de 2022_23.csv")

# Setting the URLs for each file. 
# We are retrieving the subsets with the orgnization data for the relevant year.
# In 2019 and previous years, this was subset 5 (ss5). In 2020, it is subset 7.
last_year_url <- "https://open.canada.ca/data/dataset/4301f4bb-1daa-4b50-afab-d1193b5d2284/resource/6e430653-54ee-4e96-8af3-a44c9c2cfea1/download/subset-7-sous-ensemble-7-1.csv"
this_year_url <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-2022-23/Subset%207%20-%202022_2023%20PSES%20open%20dataset%20-%20with%20labels_Sous-ensemble%207%20-%20Ensemble%20de%20donn%C3%A9es%20ouvertes%20du%20SAFF%20de%202022_23.csv"


# Download files if they don't exist.
if(!file.exists(last_year_file)) {download.file(last_year_url,last_year_file)}
if(!file.exists(this_year_file)) {download.file(this_year_url,this_year_file)}

# Load the files into R.
if(!exists("pses_last_year")) {
  pses_last_year <- read.csv(last_year_file, na.strings = "9999", encoding = "latin1")
  }
if(!exists("pses_this_year")) {
  pses_this_year <- read.csv(this_year_file, na.strings = "9999", encoding = "latin1") %>% 
    rename_with(toupper) ## Some column names now include lower case characters. We convert everything to upper case.
  }

# Load a look-up file with shortened PSES questions for this year. Some of the 
# questions are quite long, so some liberties were taken to shorten them to 
# about 100 characters in English and 120 characters in French.
short_qs <- read_excel(file.path(data_dir,"pses_2022_short_questions.xlsx"), sheet = "pses_2022") %>% 
  select(QUESTION = question, q_short_E = q_short_e, q_short_F =q_short_f)

# Create a look-up table for the question indicators.
indicatorMap <- pses_this_year %>% 
  distinct(QUESTION, TITLE_E, TITLE_F,
           INDICATORID, INDICATORENG, INDICATORFRA,
           SUBINDICATORID, SUBINDICATORENG, SUBINDICATORFRA) %>% 
  rename(INDICATOR_E = INDICATORENG,
         INDICATOR_F = INDICATORFRA,
         SUBINDICATOR_E = SUBINDICATORENG,
         SUBINDICATOR_F = SUBINDICATORFRA)

#----
# SELECT DEPARTMENT
# Run this code to see a list of departments and their LEVEL1ID codes
distinct(pses_this_year, LEVEL1ID, DEPT_E, DEPT_F)

# Enter your department's LEVEL1ID code here 
# (e.g., TBS is 26, ESDC is 2, DFO is 5, GAC is 14)
this_dept <- 14
this_dept_abbr_e <- "GAC" 
this_dept_abbr_f <- "AMC" 

#this_dept <- 26
#this_dept_abbr_e <- "TBS" 
#this_dept_abbr_f <- "SCT" 

# AVAILABLE ORGANIZATIONS
# Run the line below to see available organizations under your department
dept_orgs <- pses_this_year %>% 
  filter(LEVEL1ID == this_dept) %>% 
  distinct(BYCOND, DESCRIP_E, DESCRIP_F)

# Unfortunately, both the BYCOND and DESCRIP_E fields tend to change from
# year to year, which makes them unreliable when trying to match this year 
# to last year data. One of the reasons for this is the inherent instability
# of organizations - reorgs and name changes are not uncommon, which makes it
# excessively difficult to keep track of them year to year. 
# 
# There is no easy fix. We need to check this manually and make adjustments.
# 
# Run the code below to get all of the matched and mismatched organizations 
# for your department. This can be used as the basis for a look-up table that
# will help better bind this year and last year data together.
full_join(
  pses_this_year %>% 
    filter(LEVEL1ID == this_dept) %>% 
    distinct(BYCOND, DESCRIP_E, DESCRIP_F) %>% 
    add_column(pses_year = this_y),
  pses_last_year %>% 
    filter(LEVEL1ID == this_dept) %>%
    distinct(BYCOND, DESCRIP_E, DESCRIP_F) %>% 
    add_column(pses_year = last_y),
  by = c("DESCRIP_E", "DESCRIP_F")
)

orgs_this_year <- pses_this_year %>% 
  filter(LEVEL1ID %in% c(0, this_dept)) %>% 
  select(this_year_bycond = BYCOND, DESCRIP_E, DESCRIP_F) %>% 
  distinct()
orgs_last_year <- pses_last_year %>% 
  filter(LEVEL1ID %in% c(0, this_dept)) %>%
  select(last_year_bycond = BYCOND, DESCRIP_E, DESCRIP_F) %>% 
  distinct()

bind_rows(
  inner_join(orgs_this_year, orgs_last_year) %>% 
    mutate(
      last_year_descrip_e = DESCRIP_E, 
      this_year_descrip_e = DESCRIP_E,
      last_year_descrip_f = DESCRIP_F, 
      this_year_descrip_f = DESCRIP_F,
    ) %>% 
    select(-DESCRIP_E, -DESCRIP_F),
  anti_join(orgs_this_year, orgs_last_year) %>% 
    mutate(
      this_year_descrip_e = DESCRIP_E,
      this_year_descrip_f = DESCRIP_F
    ) %>% 
    select(-DESCRIP_E, -DESCRIP_F),
  anti_join(orgs_last_year, orgs_this_year) %>% 
    mutate(
      last_year_descrip_e = DESCRIP_E,
      last_year_descrip_f = DESCRIP_F
    ) %>% 
    select(-DESCRIP_E, -DESCRIP_F)
) %>% 
  select(
    this_year_bycond,
    this_year_descrip_e,
    this_year_descrip_f,
    last_year_bycond,
    last_year_descrip_e,
    last_year_descrip_f
  ) %>% 
  # We write the results into an output file.
  write_csv(file.path(output_dir,"orgs_this_vs_last_year.csv"))

# Once the missing entries are matched up (manually), we can read in the
# look-up table.
org_map <- read_csv(file.path(data_dir,"pses_2022_orgs_lookup.csv"))

# ABBREVIATIONS
# This next section reads a table where French and English abbreviations
# have been mapped to English sector names the DESCRIP_E field.
# You will need to construct your own, as these are not available publicly.

# Use this block to auto-generate abbreviations - not pretty, but useful.
sectorAbbr <- pses_this_year %>% 
  filter(LEVEL1ID %in% c(0,this_dept)) %>% 
  distinct(LEVEL1ID, LEVEL2ID, DESCRIP_E, DESCRIP_F) %>%
  mutate_all(stri_trans_general,"Latin-ASCII") %>% 
  mutate(abbr_E = str_remove_all(DESCRIP_E, "(?<!^|[:space:])."),
         abbr_F = abbreviate(DESCRIP_F, minlength = 2),
         abbr_E = ifelse(LEVEL1ID == this_dept & LEVEL2ID == 0,
                         this_dept_abbr_e,
                         abbr_E),
         abbr_F = ifelse(LEVEL1ID == this_dept & LEVEL2ID == 0,
                         this_dept_abbr_f,
                         abbr_F)) %>% 
  select(DESCRIP_E, abbr_E, abbr_F)

# Use this block if you have your own file. It should be in the "data" folder.
# It should have 3 columns. The format for the column headers should be:
# DESCRIP_E | abbr_E | abbr_F

# Treasury Board Secretariat
if (this_dept == 26) {
  
  my_file <- "tbs_26_abbrs.csv"
  
  sectorAbbr <- read.csv(file.path(main_dir,data_dir,my_file),
                         colClasses = c("character","character","character"),
                         col.names = c("DESCRIP_E", "abbr_E", "abbr_F"))
}

# Global Affairs Canada
if (this_dept == 14) {
  
  sectorAbbr <-  pses_this_year %>% 
    filter(LEVEL1ID %in% c(0,this_dept)) %>% 
    distinct(LEVEL1ID, LEVEL2ID, DESCRIP_E, DESCRIP_F) %>%
    mutate_all(stri_trans_general,"Latin-ASCII") %>%
    mutate(
      abbr_E = str_extract(DESCRIP_E, "[^-]+"),
      abbr_F = str_extract(DESCRIP_F, "[^-]+"),
      abbr_E = case_when(
        LEVEL1ID == this_dept & LEVEL2ID == 0 ~ this_dept_abbr_e,
        str_detect(abbr_E, "Canada:") ~ str_extract(abbr_F, "(?<=: )(.*)") %>% paste0(" HQ"),
        str_detect(abbr_E, "AKLND") ~ "OSD-C1",
        str_detect(abbr_E, "BNGKK") ~ "OSD-C2",
        str_detect(abbr_E, "AMDBD") ~ "OAD-C1",
        str_detect(abbr_E, "CLMBO") ~ "OAD-C2",
        str_detect(abbr_E, "BEJING") ~ "OPD-C1",
        str_detect(abbr_E, "CHONQ") ~ "OPD-C2",
        str_detect(abbr_E, "KYUSHU") ~ "OPD-C3",
        str_detect(abbr_E, "Abroad") ~ str_extract(abbr_F, "(?<=: )(.*)") %>% str_replace_all("\\s+", ""),
        TRUE ~ abbr_E
      ),
      abbr_F = case_when(
        LEVEL1ID == this_dept & LEVEL2ID == 0 ~ this_dept_abbr_f,
        str_detect(abbr_F, "Canada :") ~ str_extract(abbr_F, "(?<=: )(.*)") %>% paste0(" AC"),
        str_detect(abbr_F, "AKLND") ~ "OSD-G1",
        str_detect(abbr_F, "BNGKK") ~ "OSD-G2",
        str_detect(abbr_F, "AMDBD") ~ "OAD-G1",
        str_detect(abbr_F, "CLMBO") ~ "OAD-G2",
        str_detect(abbr_F, "BEJING") ~ "OPD-G1",
        str_detect(abbr_F, "CHONQ") ~ "OPD-G2",
        str_detect(abbr_F, "KYUSHU") ~ "OPD-G3",
        str_detect(abbr_F, "A l'etranger") ~ str_extract(abbr_F, "(?<=: )(.*)") %>% str_replace_all("\\s+", ""),
        TRUE ~ abbr_F
      )
    ) %>% 
    select(DESCRIP_E, abbr_E, abbr_F)
  
}


# Create Question Correspondence table. This is what we use to bind this year's
# and last year's question data across both files.
# This is derived from the "Question number correspondence" on the website:
# https://www.canada.ca/en/treasury-board-secretariat/services/innovation/public-service-employee-survey/2019-public-service-employee-survey-pses/2019-public-service-employee-survey-question-number-concordance.html

# Read raw table and designate 'this_year' and 'last_year' questions.
QCtable_raw <- read_tsv(
  file.path(data_dir,"pses_2022_question_concordance.tsv"), 
  na = "N/A"
  ) %>% 
  mutate(
    last_year = 
      ifelse(
        is.na(`2020 PSES`),
        NA,
        paste0("Q", ifelse(`2020 PSES` %in% as.character(c(1:9)),"0",""),`2020 PSES`)
      ),
    this_year = 
      ifelse(
        is.na(`2022 PSES`),
        NA,
        paste0("Q", ifelse(`2022 PSES` %in% as.character(c(1:9)),"0",""),`2022 PSES`)
      )
    ) 

# In some cases, questions with subquestions (eg, Q59a) are not included in the QC table.
# We find them and use the prefix (eg, Q59) to find the correspondence (eg, Q57) to create
# the missing entries (eg, Q57a).  

# Find questions where sub-questions have been included in the QC table (to avoid duplicates).
existing_sub_qs <- QCtable_raw %>% 
  filter(str_detect(this_year, "[A-Za-z]$")) %>% 
  mutate(this_year = str_extract(this_year, ".*(?=.$)")) %>% 
  distinct(this_year) %>% 
  pull()

# Create new correspondence questions for the missing sub-questions
missing_sub_qs <- pses_this_year %>% 
  filter(str_detect(QUESTION, "[A-Za-z]$")) %>% 
  distinct(QUESTION) %>% 
  mutate(
    this_year = str_extract(QUESTION, ".*(?=.$)"),
    q_suffix = str_extract(QUESTION, ".$")
  ) %>%
  filter(!this_year %in% existing_sub_qs) %>% 
  left_join(QCtable_raw %>% select(this_year, q_last = last_year), by = "this_year") %>% 
  drop_na(q_last) %>% 
  mutate(last_year = paste0(q_last, q_suffix)) %>% 
  select(this_year = QUESTION, last_year)

# We now combine the two previous steps into out master correspondance list.
QCtable <- QCtable_raw %>% 
  bind_rows(missing_sub_qs)



#----
# PRE-PROCESS DATA

# Get last year sector-level results and use the 'QCtable' lookup table to filter on this year questions and 
# create needed fields to merge. This uses the 'org_map' lookup table to match this year and last year orgs. 

sectors_last_year <- pses_last_year %>%
  filter(LEVEL1ID %in% c(0,this_dept)) %>%
  #mutate(QUESTION = substring(QUESTION,3)) %>%
  left_join(
    QCtable %>% select(QUESTION = last_year, Q_this_year = this_year), 
    by = "QUESTION"
  ) %>%
  left_join(
    org_map %>% select(DESCRIP_E = last_year_descrip_e, this_year_descrip_e),
    by = "DESCRIP_E"
  ) %>% 
  select(-DESCRIP_E, DESCRIP_E = this_year_descrip_e) %>% 
  drop_na(Q_this_year, DESCRIP_E) %>%
  select(DESCRIP_E,QUESTION="Q_this_year",s100_last_year="SCORE100",agree_last_year="AGREE") %>% 
  mutate(DESCRIP_E = as.character(DESCRIP_E))


# Filter this year sector-level questions and then merge last year data. 
# Gather allows us to add rows using the existing SURVEYR field.
question100s <- pses_this_year %>%
  filter(LEVEL1ID %in% c(0,this_dept)) %>%
  #left_join(select(indicatorMap,-matches("TITLE_")), by = "QUESTION") %>%
  rename(INDICATOR_E = INDICATORENG,
         INDICATOR_F = INDICATORFRA,
         SUBINDICATOR_E = SUBINDICATORENG,
         SUBINDICATOR_F = SUBINDICATORFRA) %>% 
  left_join(sectorAbbr, by = "DESCRIP_E") %>%
  left_join(short_qs, by = "QUESTION") %>% 
  #mutate(unitcode = ifelse(BYCOND == "",
  #                         ifelse(LEVEL1ID == this_dept, "dept","PS"),
  #                         word(BYCOND, 2, sep = " = "))) %>%
  mutate(unitcode = case_when(
    LEVEL1ID == 0 ~ "PS",
    LEVEL2ID == 0 ~ "dept",
    TRUE ~ word(BYCOND, 2, sep = " = ")
  )) %>% 
  left_join(sectors_last_year, by = c("QUESTION","DESCRIP_E")) %>%
  rename(s100_this_year = "SCORE100") %>%
  gather("SURVEYR","SCORE100",s100_this_year,s100_last_year) %>%
  mutate(
    SURVEYR = case_when(
      SURVEYR == "s100_this_year" ~ this_y,
      SURVEYR == "s100_last_year" ~ last_y,
    )
  ) %>%
  group_by(SURVEYR,unitcode) %>%
  mutate(overall100 = mean(SCORE100, na.rm = TRUE)) %>%
  ungroup()

# Create last yeat and this year means for the PSES indicators.
score100s <- question100s %>%
  group_by(SURVEYR,unitcode,DESCRIP_E,DESCRIP_F,abbr_E,abbr_F,INDICATORID,INDICATOR_E,INDICATOR_F,overall100) %>%
  summarise(indicator100 = mean(SCORE100, na.rm = TRUE)) %>%
  ungroup()

#----
# SET SECTOR - for testing purposes only


thisUnitcode <- "216"
thisAbbr <- "OGM"

customName <- NULL
customAbbr <- NULL

sectorData <- score100s %>%
  filter(unitcode %in% c(thisUnitcode,"dept")) %>%
  mutate(abbr_lang = ifelse(unitcode == thisUnitcode, thisAbbr, unitcode))

thisSectorName_E <- sectorData$DESCRIP_E[[1]]
#thisSectorName_E <- "Office of the Chief Information Officer"

ttl_E <- paste0("PSES 2020 Report Card - ",thisSectorName_E)

lang = "E"
#----## CONSTRUCT REPORT CARD FUNCTION

report_card <- function(thisUnitcode, lang, customName = NULL, customAbbr = NULL, question100s = question100s, score100s = score100s) {
  
  if (!(lang %in% c("F","E"))) {print("Invalid selection. Choose E or F as a language.")}
  
  # Ensure the unitcode is read as a character, not a numeric
  thisUnitcode <- as.character(thisUnitcode)
  
  # Both of the next blocks select the appropriate bilingual fields and delete
  # fields from the other language. Indicator order is also set to correspond to
  # the numeric indicator ID.
  sectorData <- score100s %>%
    as_tibble() %>% 
    filter(unitcode %in% c(thisUnitcode,"dept")) %>%
    set_names(~sub(paste0("_",lang),"_lang",.x)) %>%
    select(-matches("_[EF]")) %>% 
    mutate(INDICATOR_lang = fct_reorder(INDICATOR_lang,INDICATORID))
  
  questionData <- question100s %>% 
    as_tibble() %>% 
    filter(unitcode %in% c(thisUnitcode,"dept")) %>%
    set_names(~sub(paste0("_",lang),"_lang",.x)) %>%
    select(-matches("_[EF]")) %>% 
    mutate(INDICATOR_lang = fct_reorder(INDICATOR_lang,INDICATORID))
  
  
  # Set sectors name and abbreviation. Retrieve from dataframe by default,
  # otherwise use custom parameters.
  thisSectorName <- ifelse(is.null(customName), 
                           as.character(sectorData$DESCRIP_lang[sectorData$unitcode == thisUnitcode]),
                           customName)
  thisAbbr <- ifelse(is.null(customAbbr),
                     as.character(sectorData$abbr_lang[sectorData$unitcode == thisUnitcode]),
                     customAbbr)
  dept_abbr <- as.character(sectorData %>% filter(unitcode == "dept") %>% distinct(abbr_lang) %>% pluck(1))
  
  ps_name <- case_when(lang == "E" ~ "Public Service",
                       lang == "F" ~ "Fonction publique")
  
  # Replace existing abbreviation if a custom abbreviation was used
  sectorData <- sectorData %>% mutate(abbr_lang = ifelse(unitcode == thisUnitcode, thisAbbr, abbr_lang))
  questionData <- questionData %>% mutate(abbr_lang = ifelse(unitcode == thisUnitcode, thisAbbr, abbr_lang))
  
  # Make sure that the sector always comes before the department whenever they are compared.
  sectorData <- sectorData %>% mutate(abbr_lang = fct_relevel(abbr_lang,thisAbbr))
  questionData <- questionData %>% mutate(abbr_lang = fct_relevel(abbr_lang,thisAbbr))
  
  # Get the number of respondents for the sector
  thisAnscount <- question100s %>% 
    filter(unitcode == thisUnitcode, SURVEYR == this_y) %>% 
    drop_na(ANSCOUNT) %>% 
    summarise(ANSCOUNT = max(ANSCOUNT)) %>% 
    pull(ANSCOUNT)
  
  # Create the report card title - it includes the number of respondents.
  ttl_lang <- case_when(
    lang == "E" ~ paste0("PSES ",this_y," Report Card - ",thisSectorName," (responses = ",thisAnscount,")"),
    lang == "F" ~ paste0("Bulletin SAFF ",this_y," - ",thisSectorName," (réponses = ",thisAnscount,")")
  )
  
  #----
  ## TOP-LEFT: Consutruct slopechart comparing sector and TBS
  
  # Set slopechart theme
  slopeTheme <- list(
    theme_bw(),
    # Format tweaks
    # Remove the legend
    theme(legend.position = "none"),
    # Remove the panel border
    theme(panel.border     = element_blank()),
    # Remove just about everything from the y axis
    theme(axis.title.y     = element_blank()),
    theme(axis.text.y      = element_blank()),
    theme(panel.grid.major.y = element_blank()),
    theme(panel.grid.minor.y = element_blank()),
    # Remove a few things from the x axis and increase font size
    theme(axis.title.x     = element_blank()),
    theme(panel.grid.major.x = element_blank()),
    theme(axis.text.x.top      = element_text(size = 10, face = "bold")),
    # Remove x & y tick marks
    theme(axis.ticks       = element_blank()),
    # Format title & subtitle
    theme(text = element_text(colour = "grey30")),
    theme(plot.title       = element_text(size = 10, hjust = 0.5)),
    theme(plot.subtitle    = element_text(hjust = 0.5)),
    # Put facet labels on the left and horizontal
    theme(strip.text.y = element_text(angle = 180, size = 8)),
    theme(strip.text.x = element_text(size = 10, colour = "grey30", face = "italic")),
    theme(strip.background = element_blank())
  )
  
  # Build slopechart
  slope.plt <- ggplot(sectorData, aes(x = as.character(SURVEYR), y = round(indicator100,0), group = INDICATOR_lang)) +
    facet_grid(.~abbr_lang) + 
    geom_line(aes(color = INDICATOR_lang, linetype = abbr_lang), alpha = 0.6, size = 1) +
    scale_colour_brewer(palette = "Set2") +
    geom_text_repel(data = sectorData %>% filter(SURVEYR == last_y), 
                    aes(label = str_wrap(INDICATOR_lang,10), colour = INDICATOR_lang), 
                    hjust = 0, 
                    fontface = "bold", 
                    size = 2,
                    nudge_x = -1, 
                    direction = "y") +
    geom_text_repel(data = sectorData %>% filter(SURVEYR == this_y), 
                    aes(label = str_wrap(INDICATOR_lang,10), colour = INDICATOR_lang), 
                    hjust = 1, 
                    fontface = "bold", 
                    size = 2,
                    nudge_x = 1, 
                    direction = "y") +
    geom_point(colour = "white", size = 8, shape = 16) +
    geom_text(aes(label = round(indicator100,0), y = round(indicator100,0)),
              size = 3, colour = "grey30", fontface = "bold", alpha = 0.8) +
    scale_x_discrete(position = "top", expand = expand_scale(add = 1)) +
    scale_y_continuous(expand = expand_scale(add = 1)) +
    # Reuse theme
    slopeTheme
  
  # Create title grob
  slope.ttl_lang <- case_when(
    lang == "E" ~ "Year-to-Year Comparision (Score 100)",
    lang == "F" ~ "Comparaison annuelle (Score 100)")
  slope.ttl <- textGrob(slope.ttl_lang, gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
  space.grb <- textGrob("")
  
  # Add title to slopechart
  top_left.grb <- plot_grid(slope.ttl,
                            slope.plt,
                            space.grb,
                            nrow = 3,
                            rel_heights = c(1,11.5,0.5))
  
  #----
  ## RIGHT: Create top negative and positive shifts
  
  # This is the theme we are going to use for our right-side plots.
  
  deltaTheme <- list( 
    theme_bw(),
    # Format tweaks
    # Remove the legend
    theme(legend.position = "none"),
    # Remove the panel border
    theme(panel.border     = element_blank()),
    # Remove just about everything from the y axis
    theme(axis.title.y     = element_blank()),
    theme(axis.text.y      = element_text(size = 6, hjust = 0)), 
    theme(panel.grid.major.y = element_blank()),
    theme(panel.grid.minor.y = element_blank()),
    # Remove a few things from the x axis and increase font size
    theme(axis.title.x     = element_blank()),
    theme(panel.grid.minor.x = element_blank()),
    theme(panel.grid.major.x = element_blank()),
    theme(axis.text.x      = element_text(size = 6)),
    # Remove x & y tick marks
    theme(axis.ticks       = element_blank()),
    # Format title & subtitle
    theme(text = element_text(colour = "grey30")),
    theme(plot.title       = element_text(size = 8, hjust = 0.5)),
    theme(plot.subtitle    = element_text(hjust = 0.5)),
    # Put facet labels on the left and horizontal
    theme(strip.text.y = element_text(angle = 180, size = 6, hjust = 0)),
    theme(strip.background = element_blank())
    #theme(panel.background = element_rect(fill = "grey95")) +
  )
  
  sectorDeltas2 <- questionData %>%
    filter(unitcode == thisUnitcode) %>% 
    select(INDICATORID,INDICATOR_lang,QUESTION,TITLE_lang,q_short_lang,
           unitcode,abbr_lang,DESCRIP_lang,SURVEYR,SCORE100,AGREE) %>%
    mutate(SURVEYR = case_when(
      SURVEYR == last_y ~ "sector_last_year",
      SURVEYR == this_y ~ "sector_this_year",
      TRUE ~ as.character(SURVEYR)
    )) %>% 
    pivot_wider(names_from = SURVEYR, values_from = SCORE100) %>%
    #rename(sector_this_year = last_col(), sector_last_year = last_col(1)) %>% 
    mutate(sector_delta = sector_this_year - sector_last_year) %>% 
    left_join(question100s %>% 
                filter(unitcode == "dept", SURVEYR == this_y) %>%
                distinct(QUESTION, dept_this_year = SCORE100),
              by = "QUESTION") %>%
    left_join(question100s %>% 
                filter(unitcode == "dept", SURVEYR == last_y) %>%
                distinct(QUESTION, dept_last_year = SCORE100),
              by = "QUESTION") %>%
    mutate(dept_sector_delta = sector_this_year - dept_this_year,
           dept_delta = dept_this_year - dept_last_year) %>% 
    left_join(question100s %>% 
                filter(LEVEL1ID == 0, SURVEYR == this_y) %>%
                distinct(QUESTION, ps_this_year = SCORE100),
              by = "QUESTION")
  
  # Get the 5 best and 5 worst deltas from 2018-2019
  surveyr_deltas <- bind_rows(
    sectorDeltas2 %>% filter(sector_delta > 0) %>% arrange(desc(sector_delta),sector_this_year) %>% slice(1:5),
    sectorDeltas2 %>% filter(sector_delta < 0) %>% arrange(sector_delta,sector_this_year) %>% slice(1:5)) %>%
    mutate(ind_question = paste0(INDICATOR_lang, ": ",QUESTION, ". ", q_short_lang) %>% 
             str_wrap(width = 60) %>% fct_reorder(sector_delta),
           hjust_last_year = ifelse(sector_delta < 0, -0.3, 1.3),
           hjust_this_year = ifelse(sector_delta < 0, 1.3, -0.3),
           dir_label = ifelse(
             sector_delta < 0,
             paste0(this_y, " <<< ", last_y),#"2019 <<< 2018",
             paste0(last_y, " >>> ", this_y)#"2018 >>> 2019"
           ),
           sector_dir = case_when(sector_delta == 0 ~ "stable",
                                  sector_delta > 0 ~ "increase",
                                  sector_delta < 0 ~ "decrease"),
           dept_dir = case_when(dept_delta == 0 ~ "stable",
                                dept_delta > 0 ~ "increase",
                                dept_delta < 0 ~ "decrease")) %>%
    arrange(sector_delta)
  
  offset <- -0.3
  
  delta_clrs <- c(increase = "#4393c3", stable = "#d9d9d9", decrease = "#d6604d")
  
  surveyr_deltas.plt <- ggplot(surveyr_deltas) +
    #facet_wrap(vars(dir_label %>% fct_rev()), scales = "free", ncol = 1) +
    #facet_grid(rows = vars(direction), scales = "free_y") +
    #geom_point(aes(x = ind_question, y = sector_2019, colour = sector_dir)) +
    geom_segment(aes(x = ind_question, xend = ind_question, y = sector_last_year, yend = sector_this_year,
                     colour = sector_dir),
                 arrow = arrow(angle = 45, length = unit(3, "points"), type = "closed"), linejoin = "mitre") +
    geom_segment(aes(x = ind_question, xend = ind_question, y = dept_last_year, yend = dept_this_year,
                     colour = dept_dir),
                 position = position_nudge(x = offset),
                 arrow = arrow(angle = 45, length = unit(3, "points"), type = "open"), linejoin = "mitre") +
    geom_point(aes(x = ind_question, y = dept_last_year, shape = "b_shp"), colour = "grey50",
               fill = "white", position = position_nudge(x = offset)) +
    geom_point(aes(x = ind_question, y = sector_last_year, shape = "a_shp"), colour = "grey50") +
    #geom_point(aes(x = ind_question, y = dept_2018, colour = dept_1819_delta),
    #           shape = 124, colour = "grey60", size = 2, position = position_nudge(x = offset)) +
    geom_text(aes(label = sector_last_year, x = ind_question, y = sector_last_year, hjust = hjust_last_year),
              size = 3, colour = "grey40", fontface = "plain", vjust = 0.5) +
    geom_text(aes(label = sector_this_year, x = ind_question, y = sector_this_year,
                  colour = sector_dir, hjust = hjust_this_year),
              size = 3, fontface = "bold", vjust = 0.5) +
    geom_label(aes(label = ifelse(sector_delta > 0,
                                  paste0("+",sector_delta),
                                  sector_delta),
                   x = ind_question, y = 0, colour = sector_dir), alpha = 0.7,
               size = 3, fontface = "bold", hjust = 0, vjust = 0.5, label.size = NA, fill = "white") +
    coord_flip() +
    scale_colour_manual(values = delta_clrs,
                        guide = FALSE) +
    scale_shape_manual(name = NULL, 
                       values = c(a_shp = 19, b_shp = 21),
                       labels = c(thisAbbr, dept_abbr),
                       guide = guide_legend(nrow = 1)) +
    scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100), expand = expand_scale(add = c(0,5))) +
    deltaTheme +
    theme(strip.text = element_text(size = 8, colour = "grey40", face = "bold"),
          panel.spacing = unit(6, "points"),
          strip.background = element_rect(fill = "grey85", colour = "grey85"),
          legend.position = "top",
          legend.text = element_text(size = 6, colour = "grey50"),
          legend.margin = margin(c(0,5,0,5)),
          legend.spacing.x = unit(0.1, "cm"),
          plot.margin = unit(c(0, 1, 0.5, 0.5), units = "line"),
          panel.grid.major.y = element_line(colour = "grey95"))
  #ggsave("new_panel.pdf")
  
  # Get the 5 biggest positive and negative differences from the TBS average
  dept_deltas <- bind_rows(
    sectorDeltas2 %>% filter(dept_sector_delta > 0) %>% arrange(desc(dept_sector_delta),sector_this_year) %>% slice(1:5),
    sectorDeltas2 %>% filter(dept_sector_delta < 0) %>% arrange(dept_sector_delta,sector_this_year) %>% slice(1:5)
  ) %>%
    left_join(short_qs, by = "QUESTION") %>% 
    mutate(ind_question = paste0(INDICATOR_lang, ": ",QUESTION, ". ", q_short_lang) %>%
             str_wrap(width = 60) %>% fct_reorder(dept_sector_delta),
           hjust_dept = ifelse(dept_sector_delta < 0, -0.3, 1.3),
           hjust_sector = ifelse(dept_sector_delta < 0, 1.3, -0.3),
           dir_label = ifelse(dept_sector_delta < 0,
                              paste0(abbr_lang," <<< ",dept_abbr),
                              paste0(dept_abbr," >>> ",abbr_lang)),
           direction = case_when(dept_sector_delta == 0 ~ "stable",
                                 dept_sector_delta > 0 ~ "increase",
                                 dept_sector_delta < 0 ~ "decrease")) %>% 
    arrange(dept_sector_delta)
  
  other_sectors <- question100s %>% 
    right_join(dept_deltas %>% distinct(QUESTION,ind_question), by = "QUESTION") %>% 
    filter(! unitcode %in% c("PS","dept",thisUnitcode)) %>% 
    select(QUESTION, ind_question, unitcode, SCORE100) %>% 
    drop_na()
  
  other_name <- case_when(lang == "E" ~ paste0(dept_abbr, " sectors"),
                          lang == "F" ~ paste0("Secteurs ",dept_abbr))
  
  dept_deltas.plt <- ggplot(dept_deltas) +
    #facet_wrap(vars(dir_label %>% fct_rev()), scales = "free", ncol = 1) +
    geom_point(data = other_sectors, aes(x = ind_question, y = SCORE100, shape = "d_shp"),
               position = position_jitter(height = 0, width = 0.1),
               colour = "grey80", alpha = 0.4) +
    geom_segment(aes(x = ind_question, xend = ind_question, y = dept_this_year, yend = sector_this_year, colour = direction),
                 linetype = 3) +
    #geom_errorbar(aes(x = ind_question, ymin = dept_2019, ymax = dept_2019),
    #              colour = "grey60", linetype = 3, width = 0.8) +
    geom_point(aes(x = ind_question, y = ps_this_year, shape = "c_shp"),
               position = position_nudge(offset),
               colour = "#bc80bd") +
    geom_point(aes(x = ind_question, y = dept_this_year, colour = direction, shape = "b_shp"),
               colour = "grey50", fill = "white") +
    geom_point(aes(x = ind_question, y = sector_this_year, colour = direction, shape = "a_shp")) +
    geom_text(aes(label = dept_this_year, x = ind_question, y = dept_this_year, hjust = hjust_dept),
              size = 3, colour = "grey40", fontface = "plain", vjust = 0.5) +
    geom_text(aes(label = sector_this_year, x = ind_question, y = sector_this_year, colour = direction,
                  hjust = hjust_sector),
              size = 3, fontface = "bold", vjust = 0.5) +
    geom_label(aes(label = ifelse(dept_sector_delta > 0,
                                  paste0("+",dept_sector_delta),
                                  dept_sector_delta),
                   x = ind_question, y = 0, colour = direction), alpha = 0.7,
               size = 3, fontface = "bold", hjust = 0, vjust = 0.5, label.size = NA, fill = "white") +
    coord_flip() +
    scale_colour_manual(values = delta_clrs,
                        guide = FALSE) +
    scale_shape_manual(name = NULL, 
                       values = c(a_shp = 19, b_shp = 21, c_shp  = 17, d_shp = 18),
                       labels = c(thisAbbr, dept_abbr, ps_name, other_name),
                       guide = guide_legend(override.aes = list(colour = "grey50"), nrow = 1)) +
    scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100), expand = expand_scale(add = c(0,5))) +
    deltaTheme +
    theme(strip.text = element_text(size = 8, colour = "grey40", face = "bold"),
          panel.spacing = unit(6, "points"),
          strip.background = element_rect(fill = "grey85", colour = "grey85"),
          legend.position = "top",
          legend.text = element_text(size = 6, colour = "grey50"),
          legend.margin = margin(c(0,5,0,5)),
          legend.box.spacing = unit(0,"line"),
          legend.justification = c(0,0.5),
          legend.spacing.x = unit(0.1, "cm"),
          plot.margin = unit(c(0, 1, 0.5, 0.5), units = "line"),
          panel.grid.major.y = element_line(colour = "grey95"))
  #ggsave("new_panel.pdf")
  
# ## BELOW ARE THE ORIGINAL 2019 RIGHT-SIDE PLOTS
# 
# # Determine the deltas between PSES 2018 and PSES 2019 data
# sectorDeltas <- questionData %>%
#   #filter(unitcode %in% c(thisUnitcode, "dept")) %>%
#   #mutate(abbr_lang = ifelse(unitcode == thisUnitcode, thisAbbr, unitcode)) %>%
#   select(INDICATORID,INDICATOR_lang,QUESTION,TITLE_lang,
#          unitcode,abbr_lang,DESCRIP_lang,SURVEYR,SCORE100,AGREE) %>%
#   spread(SURVEYR,SCORE100) %>%
#   rename(this_year = last_col(), last_year = last_col(1)) %>% 
#   mutate(delta = this_year - last_year)
# 
# offset2 <- -0.3
# 
# ## REVISED- used
# best10deltas <- filter(sectorDeltas, unitcode == thisUnitcode) %>%
#   arrange(desc(delta),this_year) %>% slice(1:10) %>% select(QUESTION,delta,this_year)
# 
# bestData2 <- sectorDeltas %>%
#   inner_join(select(best10deltas,QUESTION), by = "QUESTION") %>%
#   mutate(ind_question = paste0(
#     INDICATOR_lang, ": Q", substr(TITLE_lang,10,140)
#   )) %>%
#   mutate(ind_question = str_wrap(ind_question, width = 60)) %>% 
#   mutate(org_level = ifelse(unitcode == "dept","dept","sector")) %>% 
#   select(ind_question,org_level, last_year, this_year, delta) %>% 
#   pivot_wider(names_from = org_level, values_from = c(last_year, this_year, delta)) %>% 
#   #nest(y2018, y2019, delta, .key = "value_col") %>%
#   #spread(key = org_level, value = value_col) %>% 
#   #unnest(sector,dept, .sep = "_") %>% 
#   #mutate(ind_question = fct_reorder(ind_question, delta_sector, .desc = FALSE))
#   arrange(delta_sector, last_year_sector) %>% 
#   mutate(ind_question = fct_inorder(ind_question))
# 
# best2.plt <- ggplot(data = bestData2, x = ind_question) +
#   geom_col(aes(x = ind_question, y = this_year_sector), fill = "#f7f7f7", width = 0.8) +
#   geom_errorbar(aes(x = ind_question, ymin = last_year_sector, ymax = last_year_sector), colour = "grey60", linetype = 3) +
#   geom_errorbar(aes(x = ind_question, ymin = this_year_sector, ymax = this_year_sector, colour = delta_sector)) +
#   geom_segment(aes(x = as.numeric(ind_question) + offset2, xend = as.numeric(ind_question) + offset2,
#                    y = last_year_dept, yend = this_year_dept, colour = delta_dept),
#                position = position_dodge(width = 1)) +
#   geom_point(aes(x = as.numeric(ind_question) + offset2, y = last_year_dept, colour = delta_dept),
#              position = position_dodge(width = 1), shape = 21, fill = "white") +
#   geom_point(aes(x = as.numeric(ind_question) + offset2, y = this_year_dept, colour = delta_dept),
#              position = position_dodge(width = 1)) +
#   geom_point(aes(x = ind_question, y = (last_year_sector + delta_sector/2),
#                  colour = delta_sector), shape = 62, size = 2) +
#   geom_text(aes(label = last_year_sector, x = ind_question, y = last_year_sector),
#             size = 3, colour = "grey30", fontface = "plain", hjust = 1.3, vjust = 0.5) +
#   geom_text(aes(label = this_year_sector, x = ind_question, y = this_year_sector),
#             size = 3, colour = "grey30", fontface = "bold", hjust = -0.3, vjust = 0.5) +
#   geom_text(aes(label = paste0("+",delta_sector), x = ind_question, y = 0, colour = delta_sector),
#             size = 3, fontface = "bold", hjust = 0, vjust = 0.5) +
#   coord_flip() +
#   #facet_grid(fct_reorder(paste0(INDICATOR_lang,": Q",substr(TITLE_lang,10,140)),
#   #                       delta,.desc=TRUE)~.,switch = "y", labeller = label_wrap_gen(60)) +
#   scale_colour_gradient2(high = "#0571b0", mid = "#bcbcbc", low = "#ca0020") +
#   scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100), expand = expand_scale(add = c(0,5))) +
#   deltaTheme
# 
# ## REVISED - used
# worst10deltas <- filter(sectorDeltas, unitcode == thisUnitcode) %>%
#   arrange(delta,this_year) %>% slice(1:10) %>% select(QUESTION,delta,this_year)
# 
# worstData2 <- sectorDeltas %>%
#   inner_join(select(worst10deltas,QUESTION), by = "QUESTION") %>%
#   mutate(ind_question = paste0(
#     INDICATOR_lang, ": Q", substr(TITLE_lang,10,140)
#   )) %>%
#   mutate(ind_question = str_wrap(ind_question, width = 60)) %>% 
#   mutate(org_level = ifelse(unitcode == "dept","dept","sector")) %>% 
#   select(ind_question,org_level, last_year, this_year, delta) %>%
#   pivot_wider(names_from = org_level, values_from = c(last_year, this_year, delta)) %>%
#   #nest(y2017, y2018, delta, .key = "value_col") %>%
#   #spread(key = org_level, value = value_col) %>% 
#   #unnest(sector,dept, .sep = "_") %>% 
#   #mutate(ind_question = fct_reorder(ind_question, delta_sector, .desc = TRUE))
#   arrange(desc(delta_sector), desc(this_year_sector)) %>% 
#   mutate(ind_question = fct_inorder(ind_question))
# 
# worst2.plt <- ggplot(data = worstData2, x = ind_question) +
#   geom_col(aes(x = ind_question, y = this_year_sector), fill = "#f7f7f7", width = 0.8) +
#   geom_errorbar(aes(x = ind_question, ymin = last_year_sector, ymax = last_year_sector), colour = "grey60", linetype = 3) +
#   geom_errorbar(aes(x = ind_question, ymin = this_year_sector, ymax = this_year_sector, colour = delta_sector)) +
#   geom_segment(aes(x = as.numeric(ind_question) + offset2, xend = as.numeric(ind_question) + offset2,
#                    y = last_year_dept, yend = this_year_dept, colour = delta_dept),
#                position = position_dodge(width = 1)) +
#   geom_point(aes(x = as.numeric(ind_question) + offset2, y = last_year_dept, colour = delta_dept),
#              position = position_dodge(width = 1), shape = 21, fill = "white") +
#   geom_point(aes(x = as.numeric(ind_question) + offset2, y = this_year_dept, colour = delta_dept),
#              position = position_dodge(width = 1)) +
#   geom_point(aes(x = ind_question, y = (last_year_sector + delta_sector/2),
#                  colour = delta_sector), shape = 60, size = 2) +
#   geom_text(aes(label = last_year_sector, x = ind_question, y = last_year_sector),
#             size = 3, colour = "grey30", fontface = "plain", hjust = -0.3, vjust = 0.5) +
#   geom_text(aes(label = this_year_sector, x = ind_question, y = this_year_sector),
#             size = 3, colour = "grey30", fontface = "bold", hjust = 1.3, vjust = 0.5) +
#   geom_text(aes(label = paste0(delta_sector), x = ind_question, y = 0, colour = delta_sector),
#             size = 3, fontface = "bold", hjust = 0, vjust = 0.5) +
#   coord_flip() +
#   #facet_grid(fct_reorder(paste0(INDICATOR_lang,": Q",substr(TITLE_lang,10,140)),
#   #                       delta,.desc=TRUE)~.,switch = "y", labeller = label_wrap_gen(60)) +
#   scale_colour_gradient2(high = "#0571b0", mid = "#bcbcbc", low = "#ca0020") +
#   scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100), expand = expand_scale(add = c(0,5))) +
#   deltaTheme
  
  # Create chart titles
  #best.ttl_lang <- case_when(
  #  lang == "E" ~ paste0("Top 10 Positive Shifts for ",thisAbbr," from ", last_y," to ", this_y," (Score 100)"),
  #  lang == "F" ~ paste0("Les 10 changements les plus positifs pour ",thisAbbr," de ", last_y," à ", this_y," (Score 100)"))
  #best.ttl <- textGrob(best.ttl_lang,
  #                     gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
  #
  #worst.ttl_lang <- case_when(
  #  lang == "E" ~ paste0("Top 10 Negative Shifts for ",thisAbbr," from ", last_y," to ", this_y," (Score 100)"),
  #  lang == "F" ~ paste0("Les 10 changements les plus négatifs pour ",thisAbbr," de ", last_y," à ", this_y," (Score 100)"))
  #worst.ttl <- textGrob(worst.ttl_lang,
  #                      gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
  
  surveyr_deltas.ttl_lang <- case_when(
    lang == "E" ~ paste0("Top 10 Most Extreme Shifts for ",thisAbbr," from ", last_y," to ", this_y," (Score 100)"),
    lang == "F" ~ paste0("Les 10 changements les plus extrêmes pour ",thisAbbr," de ", last_y," à ", this_y," (Score 100)"))
  surveyr_deltas.ttl <- textGrob(surveyr_deltas.ttl_lang,
                                 gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
  
  dept_deltas.ttl_lang <- case_when(
    lang == "E" ~ paste0("Top 10 Largest Differences between ",dept_abbr, " and ",thisAbbr," for ",this_y," (Score 100)"),
    lang == "F" ~ paste0("Les 10 différences les plus grandes entre ",dept_abbr," et ",thisAbbr," pour ",this_y," (Score 100)"))
  dept_deltas.ttl <- textGrob(dept_deltas.ttl_lang,
                              gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
  
  # Combine the charts and title into the right-hand pane of the report card
  #right.grb <- plot_grid(best.ttl,
  #                       best2.plt,
  #                       worst.ttl,
  #                       worst2.plt,
  #                       nrow = 4, rel_heights = c(1,12,1,12))
  
  
  
  
  
  # NEW RIGHT-HAND PANE
  right.grb <- plot_grid(surveyr_deltas.ttl,
                         surveyr_deltas.plt,
                         dept_deltas.ttl,
                         dept_deltas.plt,
                         nrow = 4, rel_heights = c(1,12,1,12))
  
  #----
  ## BOTTOM-LEFT: Create the harassment and discrimination charts
  
  # Extract the harassment and discrimination questions using the appropriate subindicators (12 and 13)
  sectorHarDis <- questionData %>%
    filter(SUBINDICATORID %in% c(16,17) & SURVEYR == this_y) %>%
    expand(QUESTION,unitcode) %>%
    left_join(distinct(select(questionData,unitcode,abbr_lang)), by = "unitcode") %>%
    left_join(distinct(select(questionData,QUESTION,TITLE_lang)), by = "QUESTION") %>%
    left_join(filter(questionData, SURVEYR == this_y), by = c("QUESTION","unitcode","abbr_lang","TITLE_lang")) %>%
    select(QUESTION,TITLE_lang,unitcode,abbr_lang,AGREE,agree_last_year,ANSCOUNT) %>%
    #mutate(abbr_lang = ifelse(unitcode == thisUnitcode, thisAbbr, unitcode)) %>%
    mutate(delta = AGREE - agree_last_year) %>%
    rename(`2020` = agree_last_year, `2022` = AGREE)
  
  sectorHarDis <- bind_rows(
    questionData %>%
      filter(QUESTION == "Q57" | str_detect(QUESTION, "Q59"), SURVEYR == this_y) %>%
      expand(QUESTION,unitcode) %>%
      left_join(distinct(select(questionData,unitcode,abbr_lang)), by = "unitcode") %>%
      left_join(distinct(select(questionData,QUESTION,TITLE_lang)), by = "QUESTION") %>%
      left_join(filter(questionData, SURVEYR == this_y), by = c("QUESTION","unitcode","abbr_lang","TITLE_lang")) %>%
      select(QUESTION,TITLE_lang,unitcode,abbr_lang,AGREE,agree_last_year,ANSCOUNT) %>%
      group_by(unitcode) %>% 
      mutate(base_q = AGREE[QUESTION == "Q57"],
             AGREE = ifelse(base_q == 0, 0, AGREE),
             delta = AGREE - agree_last_year) %>%
      rename(`2020` = agree_last_year, `2022` = AGREE) %>% 
      ungroup(),
    questionData %>%
      filter(QUESTION == "Q64" | str_detect(QUESTION, "Q66"), SURVEYR == this_y) %>%
      expand(QUESTION,unitcode) %>%
      left_join(distinct(select(questionData,unitcode,abbr_lang)), by = "unitcode") %>%
      left_join(distinct(select(questionData,QUESTION,TITLE_lang)), by = "QUESTION") %>%
      left_join(filter(questionData, SURVEYR == this_y), by = c("QUESTION","unitcode","abbr_lang","TITLE_lang")) %>%
      select(QUESTION,TITLE_lang,unitcode,abbr_lang,AGREE,agree_last_year,ANSCOUNT) %>%
      group_by(unitcode) %>% 
      mutate(base_q = AGREE[QUESTION == "Q64"],
             AGREE = ifelse(base_q == 0, 0, AGREE),
             delta = AGREE - agree_last_year) %>%
      rename(`2020` = agree_last_year, `2022` = AGREE) %>% 
      ungroup()
  )
  #mutate(i = row_number()) %>%
  #spread(unitcode,QUESTION) %>%
  #gather(key = "unitcode", value = "QUESTION", -i) %>%
  #select(-i) %>%
  
  
  #mutate(abbr_lang = ifelse(unitcode == thisUnitcode, thisAbbr, unitcode)) %>%
  #select(QUESTION,TITLE_lang,unitcode,abbr_lang,SURVEYR,AGREE,agree_2017) %>%
  #mutate(delta = AGREE - agree_2017) %>%
  #rename(`2017` = agree_2017, `2018` = AGREE)
  
  # Create the harassment and discrimination titles
  
  har.ttl_lang <- case_when(
    lang == "E" ~ "Victims of Harassment (%)",
    lang == "F" ~ "Victimes de harcèlement (%)")
  har.ttl <- textGrob(har.ttl_lang, gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
  
  dis.ttl_lang <- case_when(
    lang == "E" ~ "Victims of Discrimination (%)",
    lang == "F" ~ "Victimes de discrimination (%)")
  dis.ttl <- textGrob(dis.ttl_lang, gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
  
  har_dis.ttl <- plot_grid(har.ttl,dis.ttl)
  
  # Create a single harassment and discrimination plot showing the rates of each (Q48 and Q55)
  #har_dis.plt <- ggplot(data = filter(sectorHarDis, QUESTION %in% c("Q48","Q55")), aes(x = abbr_lang)) +
  #  geom_text(aes(label = paste0(abbr_lang,": ",`2018`,"%"),y=1, colour = QUESTION), fontface = "bold.italic", size = 4) +
  #  facet_grid(.~str_wrap(substr(TITLE_lang,10,200),50)) +
  #  scale_x_discrete(position = "bottom") +
  #  scale_colour_manual(values = c("#7fc97f","#beaed4")) +
  #  deltaTheme +
  #  theme(axis.text = element_blank()) +
  #  theme(strip.text = element_text(size = 7))
  
  suppressed <- 5/thisAnscount
  
  na_text <- case_when(lang == "E" ~ "*S*",
                       lang == "F" ~ "*S*")
  
  har_dis <- sectorHarDis %>%
    filter(QUESTION %in% c("Q57","Q64")) %>%
    select(QUESTION,abbr_lang,`2020`,`2022`) %>%
    gather("SURVEYR","AGREE",-QUESTION,-abbr_lang) %>%
    mutate(AGREE = ifelse(is.na(AGREE),suppressed,AGREE))
  
  har_dis.plt <- ggplot(har_dis, aes(x = SURVEYR, y = AGREE, group = abbr_lang)) +
    facet_wrap(~QUESTION, scales = "free_y") + 
    geom_line(data = har_dis,
              aes(linetype = abbr_lang, colour = QUESTION, alpha = abbr_lang), size = 1) +
    scale_colour_manual(values = c("#7fc97f","#beaed4")) +
    scale_alpha_manual(values = c(1,0.5)) +
    geom_text_repel(data = har_dis %>% filter(SURVEYR == last_y), 
                    aes(label = abbr_lang, colour = QUESTION, alpha = abbr_lang), 
                    hjust = 2, 
                    fontface = "bold", 
                    size = 3,
                    nudge_x = -1, 
                    direction = "y") +
    geom_text_repel(data = har_dis %>% filter(SURVEYR == this_y), 
                    aes(label = abbr_lang, colour = QUESTION, alpha = abbr_lang), 
                    hjust = -1, 
                    fontface = "bold", 
                    size = 3,
                    nudge_x = 1, 
                    direction = "y") +
    geom_point(colour = "white", size = 8, shape = 16) +
    geom_text(aes(label = ifelse(AGREE == suppressed,na_text,AGREE), y = AGREE),
              size = 3, colour = "grey30", fontface = "bold", alpha = 0.8) +
    scale_x_discrete(position = "top", expand = expand_scale(add = 1)) +
    scale_y_continuous(expand = expand_scale(add = 1)) +
    # Reuse theme
    slopeTheme +
    theme(strip.text = element_blank())
  
  # Extract the data on the nature of harassment
  harNatureData <- sectorHarDis %>%
    filter(startsWith(QUESTION,"Q59")) %>%
    #mutate(Qshort_lang = word(TITLE_lang, sep = ".")) %>% 
    mutate(Qshort_lang = word(TITLE_lang, start=3, sep = stringr::fixed('.'))) %>% 
    mutate(`2022` = ifelse(is.na(`2022`),0.5,`2022`)) %>%
    arrange(`2022`) %>%
    mutate(order = ifelse(unitcode == "dept", row_number(), NA)) 
  
  # Create the the nature of harassment chart
  harNature.plt <- ggplot(harNatureData, 
                          aes(x=fct_reorder(str_wrap(substr(Qshort_lang,1,30),30),
                                            order, .fun = max, na.rm = TRUE), y=`2022`)) +
    labs(
      x= case_when(lang == "E" ~ "Sectors", lang == "F" ~  "Secteurs"),
      y= case_when(lang == "E" ~ "% answering yes", lang == "F" ~  "% répondant oui")) +
    geom_col(aes(alpha = abbr_lang), fill = "#7fc97f") +
    geom_text(hjust=-0.1, vjust=0.5, size=3, colour="grey30", fontface = "bold", 
              aes(label=ifelse(`2022` == 0.5,na_text,`2022`), y=0)) +
    coord_flip() +
    facet_grid(.~abbr_lang) +
    scale_alpha_manual(values = c(1,.5)) +
    deltaTheme +
    theme(axis.text.y = element_text(size = 6)) +
    theme(axis.title.x = element_text(size = 8)) +
    theme(axis.text.x = element_blank()) +
    theme(strip.text = element_text(colour = "grey30", face = "italic", size = 8))
  
  # Extract the data on the type of discrimination
  disTypeData <- sectorHarDis %>%
    filter(startsWith(QUESTION,"Q66")) %>%
    mutate(Qshort_lang = word(TITLE_lang,3, sep = stringr::fixed('.'))) %>%
    mutate(`2022` = ifelse(is.na(`2022`),0.5,`2022`)) %>%
    arrange(`2022`) %>%
    mutate(order = ifelse(unitcode == "dept", row_number(), NA))
  
  # Create the type of discrimination chart
  disType.plt <- ggplot(disTypeData,
                        aes(x=fct_reorder(str_wrap(substr(Qshort_lang,1,30),30),
                                          order, .fun = max, na.rm = TRUE), y=`2022`)) +
    labs(
      x= case_when(lang == "E" ~ "Sectors", lang == "F" ~  "Secteurs"),
      y= case_when(lang == "E" ~ "% answering yes", lang == "F" ~  "% répondant oui")) +
    geom_col(aes(alpha = abbr_lang), fill = "#beaed4") +
    geom_text(hjust=-0.1, vjust=0.5, size=3, colour="grey30", fontface = "bold", 
              aes(label=ifelse(`2022` == 0.5,na_text,`2022`), y=0)) +
    coord_flip() +
    facet_grid(.~abbr_lang) +
    scale_alpha_manual(values = c(1,.5)) +
    deltaTheme +
    theme(axis.text.y = element_text(size = 6)) +
    theme(axis.title.x = element_text(size = 8)) +
    theme(axis.text.x = element_blank()) +
    theme(strip.text = element_text(colour = "grey30", face = "italic", size = 8))
  
  # Create nature of harassment and type of discrimination titles
  harNature.ttl_lang <- case_when(
    lang == "E" ~ "Nature of Harassment (2022)",
    lang == "F" ~ "Nature du harcèlement (2022)")
  harNature.ttl <- textGrob(harNature.ttl_lang,
                            gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
  
  distype.ttl_lang <- case_when(
    lang == "E" ~ "Type of Discrimination (2022)",
    lang == "F" ~ "Type de discrimination (2022)")
  disType.ttl <- textGrob(distype.ttl_lang,
                          gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
  
  na_note.ttl_lang <- case_when(
    lang == "E" ~ paste0(na_text, ": Data are suppressed because of small group size. True value is between 0 and 10 cases."),
    lang == "F" ~ paste0(na_text, " : Les données sont supprimées en raison de la petite taille du groupe. La vraie valeur est de 0 à 10 cas."))
  na_note.ttl <- textGrob(na_note.ttl_lang,
                          gp = gpar(fontsize = 8, fontface = "italic", col = "grey30"))
  
  # Combine the nature and type titles horizontally
  nature_type.ttl <- plot_grid(harNature.ttl,disType.ttl)
  
  # Combine the nature and type plots horizontally
  nature_type.plt <- plot_grid(harNature.plt,disType.plt)
  
  # Stack all of the harassment and discrimination titles and plots together to create the bottom-left panel
  bottom_left.grb <- plot_grid(har_dis.ttl,
                               har_dis.plt,
                               nature_type.ttl,
                               nature_type.plt,
                               na_note.ttl,
                               nrow=5, rel_heights = c(1,3.3,0.7,7.5,0.5))  
  
  # Combine the top-left and bottom-left grobs to create the left-hand pane of the report card
  left.grb <- plot_grid(top_left.grb,
                        bottom_left.grb,
                        nrow = 2)
  
  
  
  howto.ttl_lang <- case_when(
    lang == "E" ~ "\nHow to read this \nreport card",
    lang == "F" ~ "\nComment lire \nce bulletin"
  )
  
  limits.ttl_lang <- case_when(
    lang == "E" ~ "\nLimitations",
    lang == "F" ~ "\nLimitations"
  )
  
  howto.ttl <- textGrob(howto.ttl_lang, hjust = 0.5,
                        gp=gpar(fontsize=12,col ="grey30", fontface = "bold"))
  
  limits.ttl <- textGrob(limits.ttl_lang, hjust = 0.5,
                         gp=gpar(fontsize=12, col ="grey30", fontface = "bold"))
  
  if (lang == "E") {
    
    howto.txt <- textGrob(paste0("
This PSES \"report card\" is made up of panels summarizing
differences between the 2020 and 2022 PSES and 
between the department's average and the
",thisSectorName,".\n",
"The top-left panel shows differences between groupings of
PSES questions by theme - \"indicators\". The right panel
shows the top changes from 2020 and departmental differences.
Several different data points are highlighted to allow comparisons.\n 
The score is based on the \"Score 100\" measure for each question.
It is the average of question responses using these weights: 
Very Positive = 100, Positive = 75,
Neutral = 50,
Negative = 25, Very Negative = 0.\n
Example:
A score of 52 for \"Employee Engagement\" means that the
average \"Score 100\" of the questions under that theme
(5, 9, 10, 14, 43, 44 and 45)
for that sector was 52 out of 100 - closest to \"Neutral\".\n
Note high scores are always positive and 
low scores are always negative.\n
Example: 
For Q18,\"I feel that the quality of my work suffers because of...\", 
a score of 100 means that the work almost never suffers, while
a score of 0 indicates it almost always suffers.\n
The bottom-left panel shows a summary of discrimination and 
harassment rates and the relevant nature and type. 
Note that some data is suppressed when respondents
are 10 or less. In that case,they are replaced with \"",na_text,"\".
                            "),
hjust = 0.5, gp=gpar(fontsize=6, col ="grey30"))
    
    limits.txt <- textGrob("
Not all responses rates were the same for all sectors
(HR has this available).\n
The \"Score 100\" measure does not reflect the distribution of
responses. Example:
50 people scoring very positive and
50 people scoring very negative
(50%*100 + 50%*0 = 50)
is identical to 100 people
scoring neutral
(100%*50 = 50).\n
The \"Score 100\" measure makes assumptions based on its
weighting (described above).\n
The \"indicators\" are not stable - PSES questions change from year to year
and \"Indicator\" questions are weighted equally.\n
In cases where the rate was suppressed, only departmental data  
is presented on harassment nature and discrimination type.
                            ",
hjust = 0.5, gp=gpar(fontsize=6, col ="grey30"))
  } else if (lang == "F") {
    
    howto.txt <- textGrob(paste0("
Ce « bulletin » du SAFF se compose de panneaux résumant
les différences entre le SAFF de 2020 et celui de 2022 
et entre la moyenne du ministère et celle du
", thisSectorName,".\n ",
"Le panneau en haut à gauche montre les différences entre les groupes de
Questions sur le SAFF par thème, soit « indicateurs. » Le panneau de droite
affiche les plus grands changements depuis 2020 et différences ministérielles.
Certains points saillants sont surlignés, à titre de comparaison.\n
Le score est basé sur la mesure « Score 100 » pour chaque question.
C'est la moyenne des réponses aux questions utilisant ces poids:
Très positif = 100, Positif = 75,
Neutre = 50,
Négatif = 25, Très négatif = 0.\n
Exemple:
Un score de 52 pour « Engagement des employés » signifie que la
moyenne « Score 100 » des questions de ce thème
(5, 9, 10, 14, 43, 44 et 45)
pour ce secteur était de 52 sur 100; plus proche de « neutre.»\n
Notez que les scores élevés sont toujours positifs et les scores faibles,
toujours négatif.\n
Exemple:
Pour la Q18, 
« Je pense que la qualité de mon travail en souffre à cause de ... »
un score de 100 signifie que le travail ne souffre presque jamais,
alors qu'un score de 0 indique qu'il en souffre presque toujours.\n
Le panneau en bas à gauche montre un résumé des taux de discrimination
et de harcèlement et la nature et le type pertinents. 
Notez que certaines données sont supprimées
lorsque les répondants sont 10 ou moins.
Dans ce cas, ils sont remplacés par « ",na_text,". »
                            "),
hjust = 0.5, gp=gpar(fontsize=6, col ="grey30"))
    
    limits.txt <- textGrob("
Les taux de réponse n'étaient pas tous les mêmes pour tous 
les secteurs (ils sont disponibles chez les RH).\n
La mesure « Score 100 » ne reflète pas la distribution
des réponses. Exemple:
50 personnes marquant très positif et
50 personnes marquant très négatif
(50% * 100 + 50% * 0 = 50)
est identique à 100 personnes
marquant neutre
(100% * 50 = 50).\n
La mesure « Score 100 » fait des hypothèses basées sur sa
pondération (décrite ci-dessus).\n
Les « indicateurs » ne sont pas stables, puisque les questions
du SAFF changent d'année en année, et les questions qui les 
constituent sont pondérées également.\n
Dans les cas où le taux a été supprimé, seules les données du ministère
sont présentées sur le harcèlement et le type de discrimination.
                            ",
hjust = 0.5, gp=gpar(fontsize=6, col ="grey30"))
    
  }
  
  descrip.grb <- plot_grid(howto.ttl,howto.txt,limits.ttl,limits.txt, ncol = 1,
                           align = "v", rel_heights = c(1,10,1,6))
  
  # Combine the two panes (equal width) and the description box for the bottom.
  bottom.grb <- plot_grid(left.grb,right.grb,descrip.grb,
                          nrow = 1, rel_widths = c(5.5,5.5,3))
  
  # Create the report card title
  top.grb <- textGrob(ttl_lang, x = unit(0.01, "npc"), just= "left",
                      gp = gpar(fontsize = 14, fontface = "bold", col = "grey30"))
  
  # Combine the report and title into a single grob. All done!
  report_card.plt <- plot_grid(top.grb,
                               bottom.grb,
                               nrow=2,rel_heights = c(1,20))
  
  rc_filename <- file.path(plot_dir,paste0(ttl_lang,".pdf"))
  
  # Save the report card into a PDF
  #ggsave(rc_filename, plot = report_card.plt, height = 8.5, width = 14)
  
  #return(rc_filename)
  return(report_card.plt)
}


#----
### RUN REPORT CARDS

sectorList <- 355

sectorList <- c(207, 215, 216, 220, 321, 350:352, 359, 423:432)

report_card_dir <- "C:/Users/byron/Google Drive/GAC/PSES/PSES 2022/Report Cards/"

for (i in sectorList) { 
  # Get sector abbreviations and construct a filename
  sector_name <- as.character(score100s$DESCRIP_E[score100s$unitcode==i])
  sector_name <-  str_replace_all(sector_name, "\\\\| / |: | : ", ";")
  print(sector_name)
  rc_filename <- paste0("PSES2022 Report Cards (EN&FR) - ",i," - ",sector_name,".pdf")
  
  rc_e <- report_card(i, "E", question100s = question100s, score100s = score100s) 
  rc_f <- report_card(i, "F", question100s = question100s, score100s = score100s)
  
  # Make bilingual pdf two-pager
  pdf(file.path(report_card_dir,rc_filename),height = 8.5, width = 14,
      useDingbats=FALSE)
  #pdf(file.path(main_dir,plot_dir,paste0(i," test.pdf")),height = 8.5, width = 14)
  print(rc_e)
  print(rc_f)
  dev.off()
}



#i <- 407
#sector_name <- as.character(score100s$DESCRIP_E[score100s$unitcode==i])
#sector_name <-  str_replace_all(sector_name, "\\\\|/", ";")
#print(sector_name)
#rc_filename <- paste0("PSES2019 Report Cards (EN&FR) - ",i," - ",sector_name,".pdf")
#pdf(file.path(main_dir,plot_dir, rc_filename),
#    height = 8.5, width = 14, useDingbats = FALSE)
#report_card(i, "E", question100s = question100s, score100s = score100s) 
#report_card(i, "F", question100s = question100s, score100s = score100s)
#dev.off()

# CIOB has been rechristened OCIO, following elevation of the CIO to DM status.
# These lines ensure the new OCIO descriptors are used for CIOB's unitcode (301).
#pdf(file.path(main_dir,plot_dir,"PSES2018 Report Cards (EN&FR) - Office of the Chief Information Officer.pdf"),
#    height = 8.5, width = 14, useDingbats = FALSE)
#report_card(301, "E", "Office of the Chief Information Officer", "OCIO", question100s, score100s)
#report_card(301, "F", "Bureau du Dirigeant Principal de l'information", "BDPI", question100s, score100s)
#dev.off()
#
## GOS - 307
#pdf(file.path(main_dir,plot_dir,"PSES2018 Report Cards (EN&FR) - Government Operations Sector.pdf"),
#    height = 8.5, width = 14, useDingbats = FALSE)
#report_card(307, "E", question100s = question100s, score100s = score100s)
#report_card(307, "F", question100s = question100s, score100s = score100s)
#dev.off()
#
## CSS - 310
#pdf(file.path(main_dir,plot_dir,"PSES2018 Report Cards (EN&FR) - Corporate Services Sector.pdf"),
#    height = 8.5, width = 14, useDingbats = FALSE)
#report_card(310, "E", question100s = question100s, score100s = score100s)
#report_card(310, "F", question100s = question100s, score100s = score100s)
#dev.off()

## DFO: CFO - 340
#pdf(file.path(plot_dir,"PSES2022 Report Cards (EN&FR) - 340 - Chief Financial Officer.pdf"),
#    height = 8.5, width = 14, useDingbats = FALSE)
#report_card(340, "E", customAbbr = "CFO", question100s = question100s, score100s = score100s)
#report_card(340, "F", customAbbr = "DPF", question100s = question100s, score100s = score100s)
#dev.off()
#
## DFO: BPFM - 470
#pdf(file.path(plot_dir,"PSES2020 Report Cards (EN&FR) - 470 - NCR HQ and Regions - Budget Planning and Financial Management.pdf"),
#    height = 8.5, width = 14, useDingbats = FALSE)
#report_card(470, "E", customAbbr = "BPFM", question100s = question100s, score100s = score100s)
#report_card(470, "F", customAbbr = "PBGF", question100s = question100s, score100s = score100s)
#dev.off()
#
## DFO: FMMO - 471
#pdf(file.path(plot_dir,"PSES2020 Report Cards (EN&FR) - 471 - Financial and Materiel Management Operations.pdf"),
#    height = 8.5, width = 14, useDingbats = FALSE)
#report_card(471, "E", customAbbr = "FMMO", question100s = question100s, score100s = score100s)
#report_card(471, "F", customAbbr = "OFGM", question100s = question100s, score100s = score100s)
#dev.off()
#
## DFO: PRE - 469
#pdf(file.path(plot_dir,"PSES2020 Report Cards (EN&FR) - 469 - Chief Financial Officer's Office and Corporate Planning, Performance, Risk Management and Evaluation.pdf"),
#    height = 8.5, width = 14, useDingbats = FALSE)
#report_card(469, "E", customAbbr = "CFO-CPPRME", question100s = question100s, score100s = score100s)
#report_card(469, "F", customAbbr = "BDPF-PMRGRE", question100s = question100s, score100s = score100s)
#dev.off()




## GAC: Department
#pdf(file.path(plot_dir,"PSES2020 Report Cards (EN&FR) - Global Affairs Canada.pdf"),
#    height = 8.5, width = 14, useDingbats = FALSE)
#report_card("PS", "E", customAbbr = "PS", question100s = question100s, score100s = score100s)
#report_card("PS", "F", customAbbr = "FP", question100s = question100s, score100s = score100s)
#dev.off()
#
# 
#
## GAC: WED - 355
#pdf(file.path(plot_dir,"PSES2020 Report Cards (EN&FR) - 355 - WED.pdf"),
#    height = 8.5, width = 14, useDingbats = FALSE)
#report_card(355, "E", customAbbr = "WED", customName = "WED - Southern and Eastern Africa", question100s = question100s, score100s = score100s)
#report_card(355, "F", customAbbr = "WED", customName = "WED - Afrique australe et de l'Est", question100s = question100s, score100s = score100s)
#dev.off()
#
## GAC: ADMo / WFD / WWD - 354
#pdf(file.path(plot_dir,"PSES2020 Report Cards (EN&FR) - 354 - WGM ADMO, WFD & WWD.pdf"),
#    height = 8.5, width = 14, useDingbats = FALSE)
#report_card(354, "E", customAbbr = "ADMO/WFD/WWD", customName = "WGM ADM Office / WFD - Pan-Africa / WWD - West and Central Africa", question100s = question100s, score100s = score100s)
#report_card(354, "F", customAbbr = "BSMA/WFD/WWD", customName = "Bureau du SMA WGM / WFD - Affaires panafricaines / WWD - Afrique de l'Ouest et du Centre", question100s = question100s, score100s = score100s)
#dev.off()
#
## GAC: PRR - 354
#pdf(file.path(plot_dir,"PSES2020 Report Cards (EN&FR) - 354 - WGM ADMO, WFD & WWD.pdf"),
#    height = 8.5, width = 14, useDingbats = FALSE)
#report_card(354, "E", customAbbr = "ADMO/WFD/WWD", customName = "WGM ADM Office / WFD - Pan-Africa / WWD - West and Central Africa", question100s = question100s, score100s = score100s)
#report_card(354, "F", customAbbr = "BSMA/WFD/WWD", customName = "Bureau du SMA WGM / WFD - Affaires panafricaines / WWD - Afrique de l'Ouest et du Centre", question100s = question100s, score100s = score100s)
#dev.off()
#
## GAC: DCD - 204
#pdf(file.path(plot_dir,"PSES2020 Report Cards (EN&FR) - 204 - DCD.pdf"),
#    height = 8.5, width = 14, useDingbats = FALSE)
#report_card(204, "E", customAbbr = "DCD", customName = "DCD - Corporate Secretary", question100s = question100s, score100s = score100s)
#report_card(204, "F", customAbbr = "DCD", customName = "DCD - Secrétaire des services intégrés", question100s = question100s, score100s = score100s)
#dev.off()

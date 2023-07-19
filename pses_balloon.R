library(tidyverse)
library(broom)
library(janitor)
library(plotly)

# LOAD DATA
#------------
source("load_pses_2022.R")

this_dept <- 14

org_list <- pses_2022 %>% 
  filter(level1id == this_dept) %>% 
  distinct(level2id, level3id, level4id, level5id, bycond, descrip_e)

#org_atoms <- bind_rows(
#  org_list %>% count(level2id) %>% filter(n == 1) %>% select(unitcode = level2id),
#  org_list %>% count(level3id) %>% filter(n == 1) %>% select(unitcode = level3id),
#  org_list %>% count(level4id) %>% filter(n == 1) %>% select(unitcode = level4id),
#  org_list %>% count(level5id) %>% filter(n == 1) %>% select(unitcode = level5id)) %>% 
#  filter(!unitcode %in% c(0,999))

org_atoms <- org_list %>% 
  filter(level3id == 0, !level2id %in% c(0,999)) %>% 
  select(unitcode = level2id)

#ss5_2019_atoms <- ss5_2019 %>%
#  filter(level1id == this_dept, bycond != "") %>% 
#  mutate(unitcode = word(bycond, 2, sep = " = ") %>% as.numeric()) %>% 
#  right_join(org_atoms, by = "unitcode") %>%
#  select(-unitcode)

pses_2022_level2 <- bind_rows(
  # Part 1: Everything except subset 7 and demographic questions Q121A-K. 
  pses_2022 %>% 
    filter(
      level1id == this_dept, 
      subset != "ss7"#, 
      #!str_detect(bycond, "Q121")
    ),
  # Part 2: Compress Q121A-K into a single demographic question so that the 
  # non vis-min group is compared to all specific vis-min groups. 
  #pses_2022 %>% 
  #  filter(
  #    level1id == this_dept, 
  #    bycond %>% str_detect("Q121\\w = 1|Q120 = 2")
  #  ) %>% 
  #  mutate(dem_question = "Q121"),
  # Part 3: Only level2 groups, which should sum to 100% of the department.
  pses_2022 %>% 
    filter(level1id == this_dept, subset == "ss7") %>% 
    mutate(unitcode = word(bycond, 2, sep = " = ") %>% as.numeric()) %>% 
    right_join(org_atoms, by = "unitcode") %>%
    select(-unitcode)
)

demo_map <- 
  read_csv(
    file.path(data_dir,"pses_2022_dem_questions.csv"), 
    locale = locale(encoding = "latin1")
  ) #%>% 
  # This is the synthetic question added in "Part 2" above.
  #add_row(
  #  dem_question = "Q121",
  #  dem_question_e = "Visible minority by group",
  #  dem_question_f = "Minorité visible par groupe"
  #)

sector_abbr <- pses_2022_level2 %>% 
  filter(level1id == this_dept, dem_question == "org") %>% 
  distinct(descrip_e) %>% 
  mutate(
    abbr_e = word(descrip_e, 1, sep = " - "),
    abbr_f = word(descrip_e, 1, sep = " - ")
  )

#------------

# CALCULATE CHI-SQUARES
#------------

get_residuals <- function(df) {
  xtabs(freq ~ sentiment + bycond, data = df) %>%
    chisq.test() %>%
    augment()
}

get_pvalue <- function(df) {
  xtabs(freq ~ sentiment + bycond, data = df) %>%
    chisq.test() %>%
    tidy()
}

min_cell <- 5

chisq_data <- pses_2022_level2 %>%
  filter(level1id == as.character(this_dept) & surveyr == 2022) %>%
  #mutate(unitcode = ifelse(startsWith(bycond,"LEVEL"), 
  #                                      word(bycond, 2, sep = " = "), 
  #                                      ifelse(bycond %in% c("TBS","PS"),
  #                                             TBS.df$bycond, NA))) %>%
  #filter(!(unitcode %in% c("200","303","304","201","202","999"))) %>%
  mutate(
    dkna = (ifelse(is.na(answer6),0,answer6) + ifelse(is.na(answer7),0,answer7)),
    not_dkna_n = (100 - dkna) / 100 * anscount,
    positive_n = round(positive / 100 * not_dkna_n),
    neutral_n = round(neutral / 100 * not_dkna_n),
    negative_n = round(negative / 100 * not_dkna_n)
  ) %>% 
  filter(
    #positive_n >= min_cell, 
    #neutral_n >= min_cell, 
    #negative_n >= min_cell,
    !dem_question %in%  c("none","")
  ) %>%
  drop_na(positive_n,negative_n) %>%
  #mutate(neutral_n = replace_na(neutral_n, 0)) %>% 
  #mutate(neutral_na = ifelse(is.na(neutral_n),TRUE,FALSE)) %>%
  #filter((positive_n + negative_n)>0) %>%
  #filter(neutral_na == FALSE) %>%
  select(
    question,
    dem_question,
    bycond,
    #descrip_e,descrip_f,neutral_na
    positive_n,
    negative_n,
    neutral_n
  )

#chisq_data$bycond[chisq_data$dem_question == "TBS"] <- "TBS" 

chisq_results <- chisq_data %>%
  #filter(dem_question == "Q98") %>% 
  arrange(question, dem_question) %>% 
  group_by(question,dem_question) %>%
  gather("sentiment","freq",positive_n,negative_n,neutral_n) %>%
  drop_na(freq) %>% 
  nest() %>% 
  mutate(
    residuals = map(data, get_residuals),
    pvalue = map(data, get_pvalue)
  ) %>% 
  unnest(c(residuals, pvalue)) %>% 
  left_join(demo_map, by = "dem_question") %>% 
  mutate(
    sentiment = recode(
      sentiment, 
      negative_n = "negative", 
      neutral_n = "neutral", 
      positive_n = "positive"
    )
  )



#mutate_at(vars(bycond, sentiment), as.factor) %>%
#summarise() %>% 
#do(xtabs(freq ~ sentiment + bycond, data = .) %>% chisq.test()) %>% 
#do(function(x) xtabs(freq ~ sentiment + bycond, data = .) %>% stats::chisq.test() %>% augment()(.)) %>%  
#do((function(x) augment(chisq.test(xtabs(freq ~ sentiment + bycond, data = .))))(.)) %>%
#ungroup()

#TBSpvalues <- chisq_data %>%
#  group_by(question,dem_question) %>%
#  gather("sentiment","freq",positive_n,negative_n,neutral_n) %>%
#  do((function(x) 
#    tidy(chisq.test(xtabs(freq ~ sentiment + bycond, data = .))))(.)) %>%
#  ungroup()
#
#ChiSquares <- left_join(TBSresiduals,TBSpvalues, by = c("question","dem_question")) %>%
#  left_join(demo_map, by = "dem_question") %>% 
#  mutate(sentiment = recode(sentiment, 
#                            negative_n = "negative", 
#                            neutral_n = "neutral", 
#                            positive_n = "positive"))

dept_data <- pses_2022_level2 %>%
  filter(level1id == as.character(this_dept), surveyr == 2022#, 
         #!endsWith(bycond, c("200","303","304","201","202","999"))
  ) %>%
  gather("sentiment","prop",positive,neutral,negative) %>%
  select(
    indicator_e,
    question,
    title_e,
    bycond,
    descrip_e,
    sentiment,
    prop,anscount
  ) %>%
  left_join(
    chisq_results %>% 
      select(
        question,
        dem_question,
        dem_question_e,
        bycond,
        p.value,
        sentiment,
        .std.resid,
        .observed,
        .expected
      ),
    by = c("question","bycond","sentiment"))

demq_exclude <- c(
  "Q102", # Shift worker
  "Q103", # Full- or part-time
  "Q105", # Professioanl community
  "Q113", # Serve the public
  "Q115" # Work in a bilinguial area
  )

# Clean up some names and remove some demographic questions
dept_data_cleaned <- dept_data %>%
  # Official Languages
  mutate(descrip_e = replace(descrip_e, descrip_e == "First official language - French", "First OL: French")) %>%
  mutate(descrip_e = replace(descrip_e, descrip_e == "First official language - English", "First OL: English")) %>%
  # Number of years in public service
  mutate(descrip_e = replace(descrip_e, descrip_e == "Less than 3 years in the federal public service", "PS: <3 yrs")) %>%
  mutate(descrip_e = replace(descrip_e, descrip_e == "3 to 10 years in the federal public service", "PS: 3-10 yrs")) %>%
  mutate(descrip_e = replace(descrip_e, descrip_e == "11 to 20 years in the federal public service", "PS: 11-20 yrs")) %>%
  mutate(descrip_e = replace(descrip_e, descrip_e == "More than 20 years in the federal public service", "PS: >20 yrs")) %>%
  # Number of years in department
  mutate(descrip_e = replace(descrip_e, descrip_e == "Less than 3 years in current department or agency", "Dept: <3 yrs")) %>%
  mutate(descrip_e = replace(descrip_e, descrip_e == "3 to 10 years in current department or agency", "Dept: 3-10 yrs")) %>%
  mutate(descrip_e = replace(descrip_e, descrip_e == "11 to 20 years in current department or agency", "Dept: 11-20 yrs")) %>%
  mutate(descrip_e = replace(descrip_e, descrip_e == "More than 20 years in current department or agency", "Dept: >20 yrs")) %>% 
  # Effect of 699 on career
  mutate(descrip_e = replace(descrip_e, descrip_e == 'Taken "Other Leave with Pay" (code 699 or its equivalent) - Yes', "699: Yes")) %>%
  mutate(descrip_e = replace(descrip_e, descrip_e == 'Taken "Other Leave with Pay" (code 699 or its equivalent) - No', "699: No")) %>% 
  mutate(descrip_e = replace(descrip_e, descrip_e == 'Taken "Other Leave with Pay" (code 699 or its equivalent) since the beginning of November 2020 - Yes', "699 > Nov20: Yes")) %>%
  mutate(descrip_e = replace(descrip_e, descrip_e == 'Taken "Other Leave with Pay" (code 699 or its equivalent) since the beginning of November 2020 - No', "699 > Nov20: No")) %>% 
  # Language requirements
  mutate(descrip_e = replace(descrip_e, descrip_e == "Language requirement of position - Bilingual", "Bilingual")) %>%
  mutate(descrip_e = replace(descrip_e, descrip_e == "Language requirement of position - Unilingual English", "Unilingual English")) %>%
  mutate(descrip_e = replace(descrip_e, descrip_e == "Language requirement of position - Unilingual French", "Unilingual French")) %>%
  mutate(descrip_e = replace(descrip_e, descrip_e == "Language requirement of position - Either English or French", "English or French")) %>%
  mutate(descrip_e = replace(descrip_e, descrip_e == "Language requirement of position - Don't know", "Don't know")) %>%
  # Remove questiosn that are not of interest
  filter(!dem_question %in% demq_exclude)

bonferonni_correction <- dept_data_cleaned %>% distinct(dem_question, question) %>% drop_na(dem_question) %>% nrow()

# CREATE INTERACTIVE GRAPHIC
#------------
balloon <- dept_data_cleaned %>%
  filter(
    p.value <= 0.01/bonferonni_correction,
    .std.resid >= 3,
    !dem_question %in% demq_exclude 
  ) %>%
  #left_join(sector_abbr, by = "descrip_e") %>%
  #mutate(descrip_e = ifelse(startsWith(bycond,"LEVEL"),
  #                          as.character(abbr_e),descrip_e)) %>%
  mutate(descrip_e_cut = paste0(substr(descrip_e,1,15),"...")) %>%
  arrange(bycond) %>%
  mutate(descrip_e_cut = factor(descrip_e_cut, levels = unique(descrip_e_cut))) %>%
  mutate(question = fct_rev(question)) %>% 
  select(
    indicator_e,
    question,
    title_e,
    dem_question,
    dem_question_e,
    bycond,descrip_e,
    descrip_e_cut,
    p.value,
    sentiment,
    .std.resid,
    .observed,
    .expected,
    anscount,
    prop
  )

#balloon[balloon == "First official language - French"] <- "First OL: French"
#balloon[balloon == "First official language - English"] <- "First OL: English"

bp <- ggplot(
  data = balloon,
  aes(
    x = descrip_e_cut, 
    y = question, 
    text = paste0(
      toupper(sentiment),"<br>",
      "Observed: ",.observed," / ",anscount,", proportion = ",prop,"%","<br>",
      "Expected: ",round(.expected,0)," / ",anscount,", proportion = ",round(.expected/anscount*100,0),"%","<br>",
      title_e,"<br>",
      "Demographic: ", dem_question_e, " - ", descrip_e, "<br>",
      " (standardized residual = ",round(.std.resid,2),", p-value = ", round(p.value,5),")"
    )
  )) +
  geom_point(aes(size = .observed, colour = sentiment, alpha = .std.resid), shape = 18) +
  scale_colour_brewer(palette = "Set1") +
  scale_alpha_binned() +
  #facet_grid(rows = vars(indicator_e), cols = vars(dem_question_e), switch = "both", scales = "free",space = "free") +
  labs(
    title = "PSES@GAC 2022 - sentiment by Question and Demographic",
    subtitle = "Results of chi-square tests on sentiment responses by demogrphic for every PSES question.\nEach dot represents a significant finding (p < 0.05, standardized residual > 2).\nColours denote sentiment, size correspond to standardized residuals, opaqueness relfects the p-value.",
    caption = "Data from the 2022 Public Service Employee Survey"
  ) + 
  theme_bw() +
  theme(panel.spacing = unit(0,"mm")) +
  theme(panel.border     = element_blank()) +
  theme(strip.background = element_blank()) +
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()) +
  #theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()) +
  theme(panel.grid.major = element_line(colour = "grey90")) +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()) +
  # Format title & subtitle
  theme(plot.title       = element_text(size = 14, face = "bold", hjust = 0.5, vjust = 0)) +
  theme(plot.subtitle    = element_text(hjust = 0.5))

bp_plotly <- ggplotly(bp, tooltip = c("text")) %>% 
  layout(title = list(text = paste0(
    'PSES@GAC 2022 - sentiment by Question and Demographic',
    '<br>',
    '<sup>',
    'Based on Chi-Square Tests of sentiment and demographic group (p <= 0.01)',
    '</sup>'
  )))

htmlwidgets::saveWidget(as_widget(bp_plotly), file.path(plot_dir,"PSES2022@GAC Balloon Chart.html"))

# library(trelliscopejs)
# 
# trellis <- qplot(x = descrip_e_cut, y = question, data = balloon) +
#   theme_bw() +
#   facet_trelliscope( ~ dem_question_e,
#     nrow = 2, ncol = 6, width = 300,
#     path = "C://Users//bobyrne//AppData//Local//Temp//2//Rtmps98VDs",
#     as_plotly = TRUE, 
#     plotly_args = list(dynamicTicks = T),
#     plotly_cfg = list(displayModeBar = F)
#   )
# 
# qplot(cty, hwy, data = mpg) +
#   geom_abline(alpha = 0.5) +
#   xlim(7, 37) + ylim(9, 47) + theme_bw() +
#   facet_trelliscope(~ manufacturer + class, nrow = 2, ncol = 4)
#   
#    dem_question

################################################################################
## Load Libraries
################################################################################
pkgs <- c('purrr','plyr','tidyverse', 'plotrix','lubridate','kableExtra','hrbrthemes','ggplot2','extrafont','float','reshape',
          'gridExtra','rsvg','png','devtools','readxl','date', 'ggpubr', 'tidyselect', 'httr', 'jsonlite', 'extrafont', 'colorspace',
          'ggrepel', 'forcats', 'ggpubr', 'readstata13', 'cowplot', 'scales')
lapply(pkgs, require, character.only = TRUE)

################################################################################
## Reading in data
################################################################################

# Setting directories
r.functions.dir <- '/Users/lshvb5/Documents/rammps/'
dir.inputdata <- ('/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/UNIKIN/SurveyCTO Audits/')

# Malawi
Consentedmw <- read.csv(paste0(r.functions.dir, 'RaMMPS_MWApp/ConsentedMW.csv')) %>%
  mutate(Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2))) 

# DRC
Consented <- read.csv(paste0(dir.inputdata, "Clean data/Consented2022-08-23.csv"))%>%
  mutate(source = ifelse(grepl('Fer', Source.prov),'Feroxus',
                         ifelse(grepl('IVR', Source.prov),'IVR',NA)),
         Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2)),
         RuralUrban = case_when(E4a_lab == 'City' ~ 'Urban',
                                E4a_lab == 'Town/Trading Centre' ~ 'Urban',
                                E4a_lab == 'Rural' ~ 'Rural'))

# Burkina Faso 
dir.input <- "/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/Burkina/Survey Data/"
eval(parse(paste0(r.functions.dir,'CleaningScript_func_BF.R'), encoding="UTF-8"))
listbf <- clean_bf() #clean BF data

rdd.data <- listbf[[1]] %>%
  mutate(
    Outcome = ifelse(Outcome =='NNA'| Outcome =='NR', 'NNA/NR', as.character(Outcome)),
    Eligibility = case_when(Outcome %in% c('COMP','PART') ~ 'Eligible',
                            Outcome %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                            Outcome %in% c('REFU','NNA','NR','LANG','PEND', 'NNA or NR', 'Other') ~ 'Eligibility unknown'),
    Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Ineligible','Other Eligibility Unknown', 'CATI Other Eligibility Unknown')),
    Outcome = ifelse(Outcome =='NNA'| Outcome =='NR', 'NNA/NR', as.character(Outcome)),
    gender = case_when(gender==1 ~ "Male",
                       gender==2 ~ "Female"),
    Resp.Sex = gender)

Consentedrdd <- rdd.data %>%
  filter(Outcome == 'COMP') %>%
  dplyr::mutate(month.interview1 = lubridate::floor_date(as.Date(Date.Interview), 'month'),
                gender = case_when(is.na(gender) ~ 'Male', TRUE ~ as.character(gender)),
                Resp.Age.pyr = cut(as.numeric(age), c(14,19,29,39,49,59,64)))


################################################################################
##  Age and sex distribution
################################################################################
#MW
age_sex_mw <- Consentedmw %>%
  group_by(Resp.Age.pyr, Resp.Sex) %>%
  dplyr::summarize(population = n()) %>%
  ungroup() %>%
  group_by(Resp.Sex) %>%
  dplyr::mutate(npersex = sum(population)) %>% # sum per sex
  ungroup() %>%
  mutate(relfreq = population/npersex*100, 
         country = "MW")

ggplot(data = age_sex_mw) +
  geom_bar(stat = "identity",aes(x=Resp.Age.pyr, y = relfreq, fill=Resp.Sex), position = "dodge2") + 
  scale_fill_manual(values = c('#F8B7CD','#0671B7')) +
  ylab('Relative Frequency of Respondents (%)') + xlab('Respondent Age') +
  scale_x_discrete(labels = c('15-19','20-29','30-39','40-49','50-59','60-64')) +
  theme_bw() +
  labs(fill='Respondent Sex') +
  coord_cartesian(ylim = c(0,50)) 

ggsave("plot.png", scale = 1)

# DRC
age_sex_drc <- Consented %>%
  group_by(Resp.Age.pyr, Resp.Sex) %>%
  dplyr::summarize(population = n()) %>%
  ungroup() %>%
  group_by(Resp.Sex) %>%
  dplyr::mutate(npersex = sum(population)) %>% # sum per sex
  ungroup() %>%
  mutate(relfreq = population/npersex*100, 
         country = "DRC")

# BF
age_sex_bf <- Consentedrdd %>%
  group_by(Resp.Age.pyr, Resp.Sex) %>%
  dplyr::summarize(population = n()) %>%
  ungroup() %>%
  group_by(Resp.Sex) %>%
  dplyr::mutate(npersex = sum(population)) %>% # sum per sex
  ungroup() %>%
  mutate(relfreq = population/npersex*100, 
         country = "BF")%>%
  na.omit(age_sex_bf) # Remove rows with NA

age_sex_df <- bind_rows(age_sex_mw, age_sex_drc, age_sex_bf)

print("Current working dir: ")
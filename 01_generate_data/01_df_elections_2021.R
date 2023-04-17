rm(list = ls())
################################################################################
#Project:       migration_moldova
#Author:        Sören Schwabbauer
#Initial_Date:  11.04.23
#Purpose:       Prepare data with election results
################################################################################
################################################################################

# load libraries
pacman::p_load(haven, readxl, dplyr, labelled)

# define input, output
INPUT = paste0(getwd(), "/01_generate_data/INPUT/")
OUTPUT = paste0(getwd(), "/01_generate_data/OUTPUT/") 
TEMP = paste0(getwd(), "/01_generate_data/TEMP/")



##### election results from parliament election 2021

### retrieve data
election_results_2021 <- read_excel(paste0(INPUT, "moldova_election_results_2021.xlsx"), skip = 3)


### edit dataframe
election_results_2021 <- election_results_2021 %>% 
  
  # rename columns
  rename(
    # basic information
    "name_rai" = "Numărul circumscripției",
    "name_prim" = "Localitatea",
    "poll_id" = "Numărul secției de votare",
    
    #turnout, participation, etc.
    "regvot21jul" = "a) numărul de alegători incluși în listele electorale de bază",
    "partvot21jul" = "d) numărul de alegători care au participat la votare",
    "valvot21jul" = "h) numărul total de voturi valabil exprimate",
    
    # relevant parties
    "asvot21jul" = "Partidul Politic „Partidul Acțiune și Solidaritate”",
    "csvot21jul" = "Blocul electoral al Comuniștilor și Socialiștilor",
    "psvot21jul" = "Partidul Politic „Șor”") %>% 
  
  # recode regions 
  mutate(name_rai = case_when(name_rai == 1 ~ "MUN.CHISINAU",
                              name_rai == 2 ~ "MUN.BALTI",
                              name_rai == 4 ~ "R-UL ANENII NOI",
                              name_rai == 5 ~ "R-UL BASARABEASCA",
                              name_rai == 6 ~ "R-UL BRICENI",
                              name_rai == 7 ~ "R-UL CAHUL",
                              name_rai == 8 ~ "R-UL CANTEMIR",
                              name_rai == 9 ~ "R-UL CALARASI",
                              name_rai == 10 ~ "R-UL CAUSENI",
                              name_rai == 11 ~ "R-UL CIMISLIA",
                              name_rai == 12 ~ "R-UL CRIULENI",
                              name_rai == 13 ~ "R-UL DONDUSENI",
                              name_rai == 14 ~ "R-UL DROCHIA",
                              name_rai == 15 ~ "R-UL DUBASARI",
                              name_rai == 16 ~ "R-UL EDINET",
                              name_rai == 17 ~ "R-UL FALESTI",
                              name_rai == 18 ~ "R-UL FLORESTI",
                              name_rai == 19 ~ "R-UL GLODENI",
                              name_rai == 20 ~ "R-UL HINCESTI",
                              name_rai == 21 ~ "R-UL IALOVENI",
                              name_rai == 22 ~ "R-UL LEOVA",
                              name_rai == 23 ~ "R-UL NISPORENI",
                              name_rai == 24 ~ "R-UL OCNITA",
                              name_rai == 25 ~ "R-UL ORHEI",
                              name_rai == 26 ~ "R-UL REZINA",
                              name_rai == 27 ~ "R-UL RISCANI",
                              name_rai == 28 ~ "R-UL SINGEREI",
                              name_rai == 29 ~ "R-UL SOROCA",
                              name_rai == 30 ~ "R-UL STRASENI",
                              name_rai == 31 ~ "R-UL SOLDANESTI",
                              name_rai == 32 ~ "R-UL STEFAN VODA",
                              name_rai == 33 ~ "R-UL TARACLIA",
                              name_rai == 34 ~ "R-UL TELENESTI",
                              name_rai == 35 ~ "R-UL UNGHENI",
                              name_rai == 36 ~ "UTA GAGAUZIA",
                              name_rai == 37 ~ NA, # "Nistrului",
                              name_rai == 38 ~ NA)) %>% # external countries
  
  # filter unlabeled 
  drop_na(poll_id, name_rai) %>%
  
  # format names of communities
  mutate(name_prim = gsub("or. DROCHIA", "OR_DROCHIA", name_prim),
         name_prim = gsub("or. OCNIŢA", "OR_OCNITA", name_prim),
         name_prim = gsub("com.", "s.", name_prim), # needed to match with replication
         name_prim = gsub("[.] ", ".", name_prim),   # remove spaces in town 
         name_prim = gsub(".*[.]", "", name_prim),
         name_prim = gsub("ș", "s", name_prim),
         name_prim = gsub("ț", "t", name_prim),
         name_prim = toupper(name_prim), # all community names to uppercase
         name_prim = iconv(name_prim, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         
         # edit cityparty of chisinau
         name_prim = gsub("SECTOR BOTANICA", "CHISINAU", name_prim),
         name_prim = gsub("SECTOR BUIUCANI", "CHISINAU", name_prim),
         name_prim = gsub("SECTOR CENTRU", "CHISINAU", name_prim),
         name_prim = gsub("SECTOR CIOCANA", "CHISINAU", name_prim),
         name_prim = gsub("SECTOR RISCANI", "CHISINAU", name_prim),
         
         # communities which differ from the main df
         name_prim = gsub("CARABETOVCA", "CARABETOVKA", name_prim),  ## ist -> soll werden
         name_prim = gsub("PLOP-STIUBEI", "PLOPI-STIUBEI", name_prim),
         name_prim = gsub("POCIUMBENI", "POCUIMBENI", name_prim),
         name_prim = gsub("BUCOVAT", "BUCOVATI", name_prim),
         name_prim = gsub("VADUL- RASCOV", "VADUL-RASCOV", name_prim),
         name_prim = gsub("CIOC- MAIDAN", "CIOC-MAIDAN", name_prim)) %>%
  
  # select columns
  select(contains("21jul"), contains("name")) %>%
  
  
  # group by and aggregate over community
  group_by(name_rai, name_prim) %>%
  
  # count votes by community
  summarize(regvot21jul = sum(regvot21jul),
            partvot21jul = sum(partvot21jul),
            valvot21jul = sum(valvot21jul),
            
            asvot21jul = sum(asvot21jul),
            csvot21jul = sum(csvot21jul),
            psvot21jul = sum(psvot21jul)) %>% 
  
  # calculate relative votes
  mutate(asvot21jul = asvot21jul/valvot21jul*100,
         csvot21jul = csvot21jul/valvot21jul*100,
         psvot21jul = psvot21jul/valvot21jul*100) %>%
  
  # calculate comvot variable
  mutate(comvot21jul = csvot21jul) %>%
  
  # calcualte voter turnout
  mutate(turnout21jul = partvot21jul/regvot21jul*100) %>%
  
  # filter out the 38 lines from teh consituencies (circumscriptie)
  filter(!grepl("CIRCUMSCRIP", name_prim)) 



### add labels
var_label(election_results_2021) <- list(
  name_prim = "Community Name",
  
  
  # voting variables
  regvot21jul = "Registered voters July 2021",
  partvot21jul = "Participating voters July 2021",
  valvot21jul = "Valid votes July 2021",
  turnout21jul = "Voter turnout July 2021 (%)",
  
  # parties in General
  asvot21jul = "Party of Action and Solidarity votes July 2021 (%)",
  csvot21jul = "Electoral Bloc of Communists and Socialists votes July 2021 (%)",
  psvot21jul = "Șor Party votes July 2021 (%)",
  
  # communist votes
  comvot21jul = "Communits Party votes July 2021 (%)")




### save data
save(election_results_2021, file = paste0(TEMP, "election_results_2021.rda"))


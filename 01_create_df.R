rm(list = ls())

pacman::p_load(haven, readxl, dplyr, labelled)

INPUT = paste0(getwd(), "/data/")


### load community data from andreas
replication_community <- read_dta(paste0(INPUT, "replication_community.dta")) 
  



### election results from parliament election 2021

# to do: find unemployment rate

# download data
# to do -- download.file(url = "https://a.cec.md/ro/upload/620f7ee327cca/99908/attached_files", "test.xlsx")

moldova_results_2021 <- read_excel(paste0(INPUT, "moldova_election_results_2021.xlsx"), skip = 3)

           
moldova_results_2021 <- moldova_results_2021 %>% 
  
### rename columns
  rename(
    # basic information
    "constituency_number" = "Numărul circumscripției",
    "name_prim" = "Localitatea",
    "polling_station_number" = "Numărul secției de votare",
    "polling_station_location" = "Amplasarea secției de votare",

    #turnout, participation, etc.
    "regvot21jul" = "a) numărul de alegători incluși în listele electorale de bază",
    "partvot21jul" = "d) numărul de alegători care au participat la votare",
    "invalvot21jul" = "f) numărul buletinelor de vot declarate nevalabile",
                                        
    # relevant parties
    "asvot21jul" = "Partidul Politic „Partidul Acțiune și Solidaritate”",
    "csvot21jul" = "Blocul electoral al Comuniștilor și Socialiștilor",
    "psvot21jul" = "Partidul Politic „Șor”") %>% 
  
  
### calculate additional variables  
  mutate(valvot21jul = partvot21jul - invalvot21jul) %>% # count valid votes
  
  
### format names of communities
  mutate(name_prim = gsub("com.", "s.", name_prim), # needed to match with replication
         name_prim = gsub("[.] ", ".", name_prim),   # remove spaces in town 
         name_prim = toupper(name_prim), # all community names to uppercase
         name_prim = iconv(name_prim, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
  
  
### select columns
  select(contains("21jul"), name_prim) %>%
  

### group by and aggregate over community
  group_by(name_prim) %>%
  
  # count votes by community
  summarize(regvot21jul = sum(regvot21jul),
            partvot21jul = sum(partvot21jul),
            valvot21jul = sum(valvot21jul),
            invalvot21jul = sum(invalvot21jul),
            
            asvot21jul = sum(asvot21jul),
            csvot21jul = sum(csvot21jul),
            psvot21jul = sum(psvot21jul)) %>% 
   
  # calculate relative votes
  mutate(asvot21jul = asvot21jul/valvot21jul*100,
         csvot21jul = csvot21jul/valvot21jul*100,
         psvot21jul = psvot21jul/valvot21jul*100) %>%
  
  
### calcualte voter turnout
  mutate(turnout21jul = partvot21jul/regvot21jul*100) %>%
  
  
### filter out the 38 lines from teh consituencies (circumscriptie)
  filter(!grepl("CIRCUMSCRIP", name_prim)) 



### add labels
var_label(moldova_results_2021) <- list(
  name_prim = "Community Name",
  
  regvot21jul = "Registered voters July 2021",
  partvot21jul = "Participating voters July 2021",
  valvot21jul = "Valid votes July 2021",
  invalvot21jul = "Invalid votes July 2021",
  turnout21jul = "Voter turnout July 2021 (%)",
  
  asvot21jul = "Party of Action and Solidarity votes July 2021 (%)",
  csvot21jul = "Electoral Bloc of Communists and Socialists votes July 2021 (%)",
  psvot21jul = "Șor Party votes July 2021 (%)")


################################################################################
# merge dfs
merged <- left_join(replication_community, moldova_results_2021, by = "name_prim")
# 18 communities are in the replication_community, but not in the results 2021


# Load dataset
data <- read_dta("data/replication_community.dta")

# Define global variables
controls <- c("pop_1500_3000", "pop_3000_9999", "district_capital", "distance_capital", "dist_bc", "chisinau_balti", "pop_all_0_14_sh", "pop_all_15_34_sh", "pop_all_65_99_sh", "pop_highedu_sh", "pop_lowedu_sh", "ratio_high_low_edu", "eth_rus_sh", "eth_rus_sh2", "eth_ukr_sh", "eth_ukr_sh2", "eth_gag_sh", "eth_gag_sh2", "eth_bul_sh", "eth_bul_sh2", "eth_fract")
controls2 <- c("pop_1500_3000", "pop_3000_9999", "district_capital", "distance_capital", "dist_bc", "chisinau_balti", "pop_all_0_14_sh", "pop_all_15_34_sh", "pop_all_65_99_sh", "pop_highedu_sh", "pop_lowedu_sh", "ratio_high_low_edu", "eth_rus_sh", "eth_ukr_sh", "eth_gag_sh", "eth_bul_sh", "eth_fract")
lights <- "lights92_99"
pre_treatment1998 <- c("comvot98", "pdmvot98", "dcvot98", "pdfvot98", "turnout98")
pre_treatment1994 <- c("agrarvot94", "socialistvot94", "pibvot94", "apcdfvot94")
migrant_characteristics <- c("sh_mig_all_15_34", "sh_mig_all_fem", "sh_mig_all_high")

# Table 1: Migration patterns and results of the July 2009 parliamentary election (includes Table A4)
# Basic controls
model1 <- lm(comvot09jul ~ prev_mig_west + prev_mig_east + pop_1500_3000 + pop_3000_9999 + district_capital + distance_capital + dist_bc + chisinau_balti + pop_all_0_14_sh + pop_all_15_34_sh + pop_all_65_99_sh + pop_highedu_sh + pop_lowedu_sh + ratio_high_low_edu + eth_rus_sh + eth_rus_sh2 + eth_ukr_sh + eth_ukr_sh2 + eth_gag_sh + eth_gag_sh2 + eth_bul_sh + eth_bul_sh2 + eth_fract, data = data)
tab1_1 <- summary(model1)

# Plus pre-migration election results
model2 <- lm(comvot09jul ~ prev_mig_west + prev_mig_east + pop_1500_3000 + pop_3000_9999 + district_capital + distance_capital + dist_bc + chisinau_balti + pop_all_0_14_sh + pop_all_15_34_sh + pop_all_65_99_sh + pop_highedu_sh + pop_lowedu_sh + ratio_high_low_edu + eth_rus_sh + eth_ukr_sh + eth_gag_sh + eth_bul_sh + eth_fract + comvot98 + pdmvot98 + dcvot98 + pdfvot98 + turnout98 + agr
             


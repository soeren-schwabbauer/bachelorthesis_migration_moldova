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


################################################################################
################################################################################
# to do: download the file in R
##### election results from parliament election 2021

### retrieve data
moldova_results_2021 <- read_excel(paste0(INPUT, "moldova_election_results_2021.xlsx"), skip = 3)

### edit dataframe
moldova_results_2021 <- moldova_results_2021 %>% 
  
# rename columns
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
  
  
# calculate additional variables  
  mutate(valvot21jul = partvot21jul - invalvot21jul) %>% # count valid votes
  
  
# format names of communities
  mutate(name_prim = gsub("com.", "s.", name_prim), # needed to match with replication
         name_prim = gsub("[.] ", ".", name_prim),   # remove spaces in town 
         name_prim = toupper(name_prim), # all community names to uppercase
         name_prim = iconv(name_prim, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
  
  
# select columns
  select(contains("21jul"), name_prim) %>%
  

# group by and aggregate over community
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
  
  # calculate comvot variable
  mutate(comvot21jul = csvot21jul) %>%
  
  
# calcualte voter turnout
  mutate(turnout21jul = partvot21jul/regvot21jul*100) %>%
  
  
# filter out the 38 lines from teh consituencies (circumscriptie)
  filter(!grepl("CIRCUMSCRIP", name_prim)) 



### add labels
var_label(moldova_results_2021) <- list(
  name_prim = "Community Name",
  
  # voting variables
  regvot21jul = "Registered voters July 2021",
  partvot21jul = "Participating voters July 2021",
  valvot21jul = "Valid votes July 2021",
  invalvot21jul = "Invalid votes July 2021",
  turnout21jul = "Voter turnout July 2021 (%)",
  
  # parties in General
  asvot21jul = "Party of Action and Solidarity votes July 2021 (%)",
  csvot21jul = "Electoral Bloc of Communists and Socialists votes July 2021 (%)",
  psvot21jul = "Șor Party votes July 2021 (%)",
  
  # communist votes
  comvot21jul = "Communits Party votes July 2021 (%)")


################################################################################
################################################################################
# load community data from andreas
replication_community <- read_dta(paste0(INPUT, "replication_community.dta")) 


# merge dfs
community_data <- left_join(replication_community, moldova_results_2021, by = "name_prim")
# 18 communities are in the replication_community, but not in the results 2021


################################################################################
################################################################################
### save data
save(community_data, file = paste0(OUTPUT, "community_data.rda"))

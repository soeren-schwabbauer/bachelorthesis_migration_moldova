rm(list = ls())
################################################################################
#Project:       migration_moldova
#Author:        Sören Schwabbauer
#Initial_Date:  11.04.23
#Purpose:       Prepare data with election results from 2014
# # link to data: https://a.cec.md/ro/rezultatele-alegerilor-excel-3926.html
################################################################################
################################################################################

# load libraries
pacman::p_load(haven, readxl, tidyverse, labelled)

# define input, output
INPUT = paste0(getwd(), "/01_generate_data/INPUT/")
OUTPUT = paste0(getwd(), "/01_generate_data/OUTPUT/") 
TEMP = paste0(getwd(), "/01_generate_data/TEMP/")



##### election results from parliament election 2021

## get names of rai
name_rai <- excel_sheets(paste0(INPUT, "moldova_election_results_2014.xls")) 
## empty list for each rai
results <- list()

## loop to get edit each rai
for(i in name_rai){
  
  # import data
  results[[i]] <- read_excel(paste0(INPUT, "moldova_election_results_2014.xls"), sheet = i, skip = 1) %>%
    
    # 1st row is empty
    filter(!row_number() == 1) %>%
    
    # only column 2 contains character string, rest is numeric
    mutate_at(c(1, 3:35), as.numeric) %>%
    
    # add region names
    mutate(name_rai = i) %>%
    
    # rename columns  
    rename("name_prim" = contains("Localitatea"),
           
           #turnout, participation, valid votes
           "regvot14nov" = "Numărul de alegători incluşi în listele electorale de bază",
           "partvot14nov" = "Numărul de alegători care au participat la votare",
           "valvot14nov" = "Numărul total de voturi valabil exprimate pentru fiecare concurent electoral",
           
           # relevant parties
           "psrmvot14nov" = "Partidul Politic „Partidul Socialiştilor din Republica Moldova”",
           "pldmvot14nov" = "Partidul Liberal",
           "pcvot14nov" = "Partidul Comuniştilor din Republica Moldova",
           "psde14nov" = "Partidul Democrat din Moldova",
           "pl14nov" = "Partidul Liberal Democrat din Moldova") %>%
    
    # drop irrelevant column 
    drop_na(c(1,2)) %>%
    
    # select variables
    select(contains("14nov"), contains("name")) %>%
    
    # edit community names
    mutate(name_prim = gsub("ț", "t", name_prim),
           name_prim = gsub("Ș", "S", name_prim),
           name_rai = gsub("Ș", "S", name_rai),
           name_prim = gsub("Ț", "T", name_prim),
           name_prim = gsub("ş", "s", name_prim),
           name_prim = gsub("ș", "s", name_prim),
           
           name_prim = iconv(name_prim, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
           name_rai = iconv(name_rai, from = "UTF-8", to = "ASCII//TRANSLIT"),
           name_prim = toupper(name_prim),
           
           name_rai = case_when(name_rai == "CHISINAU" ~ "MUN.CHISINAU",
                                name_rai == "BALTI" ~ "MUN.BALTI",
                                name_rai == "ANENII NOI" ~ "R-UL ANENII NOI",
                                name_rai == "BASARABEASCA" ~ "R-UL BASARABEASCA",
                                name_rai == "BRICENI" ~ "R-UL BRICENI",
                                name_rai == "CAHUL" ~ "R-UL CAHUL",
                                name_rai == "CANTEMIR" ~ "R-UL CANTEMIR",
                                name_rai == "CALARASI" ~ "R-UL CALARASI",
                                name_rai == "CAUSENI" ~ "R-UL CAUSENI",
                                name_rai == "CIMISLIA" ~ "R-UL CIMISLIA",
                                name_rai == "CRIULENI" ~ "R-UL CRIULENI",
                                name_rai == "DONDUSENI" ~ "R-UL DONDUSENI",
                                name_rai == " DROCHIA" ~ "R-UL DROCHIA",
                                name_rai == "DUBASARI" ~ "R-UL DUBASARI",
                                name_rai == "EDINET" ~ "R-UL EDINET",
                                name_rai == "FALESTI" ~ "R-UL FALESTI",
                                name_rai == "FLORESTI" ~ "R-UL FLORESTI",
                                name_rai == "GLODENI" ~ "R-UL GLODENI",
                                name_rai == "HINCESTI" ~ "R-UL HINCESTI",
                                name_rai == "IALOVENI" ~ "R-UL IALOVENI",
                                name_rai == "LEOVA" ~ "R-UL LEOVA",
                                name_rai == "NISPORENI" ~ "R-UL NISPORENI",
                                name_rai == "OCNITA" ~ "R-UL OCNITA",
                                name_rai == "ORHEI" ~ "R-UL ORHEI",
                                name_rai == "REZINA" ~ "R-UL REZINA",
                                name_rai == "RISCANI" ~ "R-UL RISCANI",
                                name_rai == "SINGEREI" ~ "R-UL SINGEREI",
                                name_rai == "SOROCA" ~ "R-UL SOROCA",
                                name_rai == "STRASENI" ~ "R-UL STRASENI",
                                name_rai == "SOLDANESTI" ~ "R-UL SOLDANESTI",
                                name_rai == "STEFAN VODA" ~ "R-UL STEFAN VODA",
                                name_rai == "TARACLIA" ~ "R-UL TARACLIA",
                                name_rai == "TELENESTI" ~ "R-UL TELENESTI",
                                name_rai == "UNGHENI" ~ "R-UL UNGHENI",
                                name_rai == "UTA GAGAUZIA" ~ "UTA GAGAUZIA",
                                name_rai == 37 ~ NA, # "Nistrului",
                                name_rai == 38 ~ NA), # external countries
           
           name_prim = gsub("BOTANICA CHISINAU", "CHISINAU", name_prim),
           name_prim = gsub("BUIUCANI CHISINAU", "CHISINAU", name_prim),
           name_prim = gsub("CENTRU CHISINAU", "CHISINAU", name_prim),
           name_prim = gsub("CIOCANA CHISINAU", "CHISINAU", name_prim),
           name_prim = gsub("RISCANI CHISINAU", "CHISINAU", name_prim),
           
           # manual edits of community names, since they are different to the replication df
           name_prim = gsub("CARABETOVCA", "CARABETOVKA", name_prim),  ## ist -> soll werden
           name_prim = gsub("SLOBOZIA- SIRAUTI", "SLOBOZIA-SIRAUTI", name_prim),
           name_prim = gsub("BAURCI- MOLDOVENI", "BAURCI-MOLDOVENI", name_prim),
           name_prim = gsub("ZARNESTI", "ZIRNESTI", name_prim),
           name_prim = gsub("TIRGUL VERTIUJENI", "TIRGUL-VERTIUJENI", name_prim),
           name_prim = gsub("CHIRCAIESTI", "CHIRCAIESTII NOI", name_prim),
           name_prim = gsub("PLOP STIUBEI", "PLOPI-STIUBEI", name_prim),
           name_prim = gsub("CIUCUR MINGIR", "CIUCUR-MINGIR", name_prim),
           name_prim = gsub("HIRTOPUL  MARE", "HIRTOPUL MARE", name_prim),
           name_prim = gsub("SLOBOZIA - DUSCA", "SLOBOZIA-DUSCA", name_prim),
           name_prim = gsub("COTUL-MORII", "COTUL MORII", name_prim),
           name_prim = gsub("FUNDUL-GALBENEI", "FUNDUL GALBENEI", name_prim),
           name_prim = gsub("SARATA GALBENA", "SARATA-GALBENA", name_prim),
           name_prim = gsub("SARATA RAZESI", "SARATA-RAZESI", name_prim),
           name_prim = gsub("TOCHILE RADUCANI", "TOCHILE-RADUCANI", name_prim),
           name_prim = gsub("VALEA TRESTIENI", "VALEA-TRESTIENI", name_prim),
           name_prim = gsub("FRUNZE", "FRUNZA", name_prim),
           name_prim = gsub("GRINAUTI MOLDOVA", "GRINAUTI-MOLDOVA", name_prim),
           name_prim = gsub("BOLOHANI", "BOLOHAN", name_prim),
           name_prim = gsub("PRIPICENI- RAZESI", "PRIPICENI-RAZESI", name_prim),
           name_prim = gsub("SAHARNA-NOUA", "SAHARNA NOUA", name_prim),
           name_prim = gsub("MALINOVSCOIE", "MALINOVSCOE", name_prim),
           name_prim = gsub("POCIUMBENI", "POCUIMBENI", name_prim),
           name_prim = gsub("CIUCIUENI", "CIUCIUIENI", name_prim),
           name_prim = gsub("REDI CERESNOVAT", "REDI-CERESNOVAT", name_prim),
           name_prim = gsub("SCHINENII NOI", "SCHINENI", name_prim),
           name_prim = gsub("SEPTILICI", "SEPTELICI", name_prim),
           name_prim = gsub("BUCOVAT", "BUCOVATI", name_prim),
           name_prim = gsub("VAD RASCOV", "VADUL-RASCOV", name_prim),
           name_prim = gsub("MUSIATU", "MUSAITU", name_prim),
           name_prim = gsub("CODRU NOU", "CODRUL NOU", name_prim),
           name_prim = gsub("PISTRUENI", "PISTRUIENI", name_prim),
           name_prim = gsub("CASLITA-PRUT", "CISLITA-PRUT", name_prim),
           name_prim = gsub("BEJGHIOZ", "BESGHIOZ", name_prim),
           name_prim = gsub("CAZACLIEA", "CAZACLIA", name_prim),
           # name_prim = case_when(name_prim == "CHIOSELIA" ~ "CHIOSELIA RUSA", .default = as.character(name_prim)),
           name_prim = gsub("CHIRSOVO", "CHIRSOVA", name_prim),
           name_prim = gsub("CONGAZCIC", "CONGAZCICUL DE SUS", name_prim),
           name_prim = gsub("DEZGHINJA", "DEZGHINGEA", name_prim),
           name_prim = gsub("ETULIEA", "ETULIA", name_prim),
           name_prim = gsub("GAIDARI", "GAIDAR", name_prim),
           name_prim = gsub("CARBOLIEA", "CARBALIA", name_prim),
           name_prim = gsub("JOLTAI", "DJOLTAI", name_prim),
           name_pimt = gsub("DDJOLTAI", "JOLTAI", name_prim)) %>%
    
    # drop_na
    drop_na(name_rai) %>%
    
    # group & aggregate votes
    group_by(name_rai, name_prim) %>%
    
    # count votes by community
    summarize(regvot14nov = sum(regvot14nov),
              partvot14nov = sum(partvot14nov),
              valvot14nov = sum(valvot14nov),
              valvot14nov = sum(valvot14nov),
              
              psrmvot14nov = sum(psrmvot14nov),
              pldmvot14nov = sum(pldmvot14nov),
              pcvot14nov = sum(pcvot14nov),
              psde14nov = sum(psde14nov),
              pl14nov = sum(pl14nov)) %>% 
    
    # calculate relative votes
    mutate(psrmvot14nov = psrmvot14nov/valvot14nov * 100,
           pldmvot14nov = pldmvot14nov/valvot14nov * 100,
           pcvot14nov = pcvot14nov/valvot14nov * 100,
           psde14nov = psde14nov/valvot14nov * 100,
           pl14nov = pl14nov/valvot14nov * 100 ) %>%
    
    # Party of socialists & party of communitsts are both communistic countries
    mutate(comvot14nov = psrmvot14nov + pcvot14nov) %>%
    
    # calculate voter turnout
    mutate(turnout14nov = partvot14nov/regvot14nov*100) 
  
}

election_results_2014 <- bind_rows(results)

### add labels
var_label(election_results_2014) <- list(
  name_prim = "Community Name",
  
  # voting variables
  regvot14nov = "Registered voters November 2014",
  partvot14nov = "Participating voters November 2014",
  valvot14nov = "Valid votes November 2014",
  turnout14nov = "Voter turnout November 2014 (%)",
  
  # parties in General
  psrmvot14nov = "Party of Socialists votes November 2014 (%)",
  pldmvot14nov = " Liberal Democratic votes November 2014 (%)",
  pcvot14nov = "Party of Communists votes November 2014 (%)",
  psde14nov = "European Social Democratic Party November 2014 (%)",
  pl14nov = "Liberal Party votes November 2014 (%)",
  
  
  # communist votes
  comvot14nov = "Communits Party votes November 2014 (%)")



### save data
save(election_results_2014, file = paste0(TEMP, "election_results_2014.rda"))


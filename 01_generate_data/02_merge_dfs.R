rm(list = ls())
################################################################################
################################################################################
#Project:       migration_moldova
#Author:        Sören Schwabbauer
#Initial_Date:  13.04.23
#Purpose:       
################################################################################
################################################################################
pacman::p_load(tidyverse, haven)

# define input, output
INPUT = paste0(getwd(), "/01_generate_data/INPUT/")
OUTPUT = paste0(getwd(), "/01_generate_data/OUTPUT/") 
TEMP = paste0(getwd(), "/01_generate_data/TEMP/")

# load election data
load(paste0(TEMP, "election_results_2021.rda"))

load(paste0(TEMP, "election_results_2014.rda"))

## merge the new election dataframes

# load community data from andreas
replication_community <- read_dta(paste0(INPUT, "replication_community.dta")) %>%
  
  
  mutate(name_prim = gsub("OR.DROCHIA", "OR_DROCHIA", name_prim),
         name_prim = gsub("OR.OCNITA", "OR_OCNITA", name_prim),
         name_prim = gsub(".*[.]", "", name_prim))


################################################################################
# find out difference in names
replication_community_names <- replication_community %>% select(name_prim, name_rai, valvotes09jul)

### difference in community names from 2021 to 2009
diff_2021 <- left_join(replication_community, election_results_2021, by = c("name_prim", "name_rai")) %>%
  
  select(name_rai, name_prim, contains("valvot")) %>%
  
  filter(!complete.cases(valvot21jul))

### diffenrence in community names from 2014 to 2009
diff_2014 <- left_join(replication_community, election_results_2014, by = c("name_prim", "name_rai")) %>%
  
  select(name_prim, name_rai, contains("valvot")) %>%
  
  filter(!complete.cases(valvot14nov))
## 14 villages, that can't be matched
################################################################################
# merge
# merge dfs
community_data <- left_join(replication_community, election_results_2021, by = c("name_prim", "name_rai")) %>%
  left_join(., election_results_2014, by = c("name_prim", "name_rai"))


diff_all <- community_data %>% filter(!complete.cases(valvot14nov) | !complete.cases(valvot21jul))
# -> 11 communities did not match


community_data %>% group_by(name_prim, name_rai) %>% summarise(n = n()) %>% filter(n >= 2)
# no double occourances

### save data
save(community_data, file = paste0(OUTPUT, "community_data.rda"))

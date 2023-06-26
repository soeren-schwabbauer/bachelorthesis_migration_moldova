rm(list = ls())
################################################################################
#Project:       migration_moldova
#Author:        Sören Schwabbauer
#Initial_Date:  11.04.23
#Purpose:       replicate results from "The Effect of Labor Migration on the 
#               Diffusion of Democracy: Evidence from a Former Soviet Republic"
#               with the new election results as explanatory variable
################################################################################

# load libraries
library(tidyr)
library(dplyr)
library(haven)
library(data.table)
library(gridExtra)
library(ggplot2)
library(rlang)
# define input, output
INPUT = paste0(getwd(), "/04_DataBuild/INPUT/")
OUTPUT = paste0(getwd(), "/04_DataBuild/OUTPUT/") 


# Load dataset
load(paste0(INPUT, "community_data.rda"))

# Define global variables
controls <- c("pop_1500_3000", "pop_3000_9999", "district_capital", "distance_capital", "dist_bc", "chisinau_balti", "pop_all_0_14_sh", "pop_all_15_34_sh", "pop_all_65_99_sh", "pop_highedu_sh", "pop_lowedu_sh", "ratio_high_low_edu", "eth_rus_sh", "eth_rus_sh2", "eth_ukr_sh", "eth_ukr_sh2", "eth_gag_sh", "eth_gag_sh2", "eth_bul_sh", "eth_bul_sh2", "eth_fract")
controls2 <- c("pop_1500_3000", "pop_3000_9999", "district_capital", "distance_capital", "dist_bc", "chisinau_balti", "pop_all_0_14_sh", "pop_all_15_34_sh", "pop_all_65_99_sh", "pop_highedu_sh", "pop_lowedu_sh", "ratio_high_low_edu", "eth_rus_sh", "eth_ukr_sh", "eth_gag_sh", "eth_bul_sh", "eth_fract")
lights <- "lights92_99"
pre_treatment1998 <- c("comvot98", "pdmvot98", "dcvot98", "pdfvot98", "turnout98")
pre_treatment1994 <- c("agrarvot94", "socialistvot94", "pibvot94", "apcdfvot94")
migrant_characteristics <- c("sh_mig_all_15_34", "sh_mig_all_fem", "sh_mig_all_high")


################################################################################
################################################################################
# FIGURE 1: COMMUNIST VOTES, NUMBER OF EMIGRANTS IN STOCKS, AND VOLUME OF CALLS FROM ABROAD TO MOLDOVA, 1998–2009
################################################################################
################################################################################

# define community types
community_data <- community_data %>% mutate(prev_mig_west_q2 = ifelse(prev_mig_west > median(prev_mig_west), 2, 1),
                                            prev_mig_east_q2 = ifelse(prev_mig_east > median(prev_mig_east), 2, 1),
                                            sh_mig_west_q2 = ifelse(sh_mig_west > 50, 2, 1)) %>%
  rename(comvot1998 = comvot98,
         comvot2001 = comvot01,
         comvot2005 = comvot05,
         comvot2009 = comvot09jul,
         comvot2014 = comvot14nov,
         comvot2021 = comvot21jul) %>%
  
  drop_na(comvot2021, comvot2014)


vars <- c("comvot1998", "comvot2001", "comvot2005", "comvot2009", "comvot2014", "comvot2021")
comvot <- list()
for(i in vars){
  
  # define community types
  community_data <- community_data %>% mutate(prev_mig_west_q2 = ifelse(prev_mig_west > median(prev_mig_west), 2, 1),
                                              prev_mig_east_q2 = ifelse(prev_mig_east > median(prev_mig_east), 2, 1),
                                              sh_mig_west_q2 = ifelse(sh_mig_west > 50, 2, 1))
  
  
  
  # create formular for linear model
  formula <- paste(i, "~", paste(controls, collapse = "+"), "+", paste(lights, collapse = "+"), "+", paste(pre_treatment1994, collapse = "+")) %>% formula()
  
  # get residuals & merge to df
  community_data$comvot_r <- lm(formula, data = community_data)$residuals
  
  # all communities
  all_comvot <- community_data %>% pull(i) %>% mean()
  
  west_comvot <- community_data %>% 
    # west communities
    filter(sh_mig_west_q2 == 2 & prev_mig_west_q2 == 2) %>%
    #  # calculate variable
    pull(comvot_r) %>% mean() + all_comvot
  
  east_comvot <- community_data %>% #
    # east communities
    filter(sh_mig_west_q2 == 1 & prev_mig_east_q2 == 2) %>%
    # calculate variable
    pull(comvot_r) %>% mean() + all_comvot
  
  comvot[[i]] <- list(all_comvot = all_comvot, west_comvot = west_comvot, east_comvot = east_comvot)
}

# unbind list
comvot <- mapply(`[<-`, comvot, 'year', value = names(comvot), SIMPLIFY = FALSE)
comvot <- rbindlist(comvot) %>% 
  
  # transform to long table
  pivot_longer(cols = ends_with("comvot"), 
               names_to = "comvot") %>%
  
  # remove comvot
  mutate(year = gsub("comvot", "", year),
         year = as.numeric(year))


save(comvot, file = paste0(OUTPUT, "elections_comvot.rda"))

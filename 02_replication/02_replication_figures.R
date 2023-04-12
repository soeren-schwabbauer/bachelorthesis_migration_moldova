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
pacman::p_load(tidyverse, haven, data.table)

# define input, output
INPUT = paste0(getwd(), "/02_replication/INPUT/")
#OUTPUT = paste0(getwd(), "/01_generate_data/OUTPUT/") 


# Load dataset
load(paste0(INPUT, "community_data.rda"))
migrants_calls <- read_dta(paste0(INPUT, "migrants_calls.dta"))


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
         comvot2009 = comvot09jul)


vars <- c("comvot1998", "comvot2001", "comvot2005", "comvot2009")
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
  

coeff <- 10
ggplot(comvot) +
  
  # com votes
  geom_point(aes(y = value, x = year, colour = comvot)) +


  geom_line(aes(y = value, x = year, color = comvot, linetype = comvot)) +


  # emigrants
  geom_bar(data = migrants_calls, aes(y = migrants/10, x = year), fill = "#D3D3D3", stat = "identity") +
  
  # calls
  geom_line(data = migrants_calls, aes(y = incoming/10, x = year), colour = "#FFA500", stat = "identity") +
  geom_point(data = migrants_calls, aes(y = incoming/10, x = year), colour = "#FFA500") +

  
  # axis settings
  scale_x_continuous(breaks = c(1998, 2001, 2005, 2009), name = "Year of parliamentary election") +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60),
                     name = "Share of Communist votes (%)",
                     
                     sec.axis = ggplot2::sec_axis(~.*10, name = "1000 emigrants / 1000 hours per week", breaks = c(0,100,200,300,400))) +

  
  scale_linetype_manual(values = c("all_comvot"="solid" ,
                                   "east_comvot"="dashed",
                                   "west_comvot" = "dotdash")) +
  
  scale_color_manual(values= c("all_comvot"="black" ,
                               "east_comvot"="red",
                               "west_comvot" = "blue")) +
  
  
  # legend
  scale_linetype_discrete(labels = c("communist votes in all communities", "Communits votes in the communities with high level of emigration the the East", "Communits votes in the communities with high level of emigration the the West")) +
  scale_shape_discrete(labels = c("communist votes in all communities", "Communits votes in the communities with high level of emigration the the East", "Communits votes in the communities with high level of emigration the the West")) +
  scale_colour_discrete(labels = c("communist votes in all communities", "Communits votes in the communities with high level of emigration the the East", "Communits votes in the communities with high level of emigration the the West")) +
  guides(linetype = guide_legend(nrow = 3),
         shape = guide_legend(nrow = 3),
         colour = guide_legend(nrow = 3)) +
  labs(linetype = NULL, shape = NULL, colour = NULL,) +
  
  # label financial crisis
  geom_vline(xintercept = 1998.8) +
  annotate("text", x=1998.9, y=60, label= "Russian financial cirsis ~ begin of emigration", hjust = 0) +
  
  # label na 
  annotate("text", x=1998, y=2, label= "n.a.") +
  
  theme_classic() +
  
  theme(legend.position = "bottom") 
             
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
pacman::p_load(tidyverse, haven, data.table, gridExtra)

# define input, output
INPUT = paste0(getwd(), "/03_renew_paper/INPUT/")
OUTPUT = paste0(getwd(), "/03_renew_paper/OUTPUT/") 


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


fig_1 <- ggplot(comvot) +
  
  # com votes
  geom_point(aes(y = value, x = year, colour = comvot), size = 2.5) +
  geom_line(aes(y = value, x = year, color = comvot, linetype = comvot), size = 1.5) +
  
  
  # emigrants
  geom_bar(data = migrants_calls, aes(y = migrants/10, x = year), fill = "#D3D3D3", stat = "identity") +
  
  # calls
  geom_point(data = migrants_calls, aes(y = incoming/10, x = year), fill = "#FFA500", color = "#FFA500", shape = 23, size = 2) +
  geom_line(data = migrants_calls, aes(y = incoming/10, x = year), colour = "#FFA500", stat = "identity", size = 1) +
  
  
  # axis settings
  scale_x_continuous(breaks = c(1998, 2001, 2005, 2009, 2014, 2021), name = "Year of parliamentary election") +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60),
                     name = "Share of Communist votes (%)",
                     
                     sec.axis = ggplot2::sec_axis(~.*10, name = "1000 emigrants / 1000 hours per week", breaks = c(0,100,200,300,400))) +
  
  
  
  
  # legend
  scale_linetype_manual(values = c("all_comvot"="solid" , "east_comvot"="dashed", "west_comvot" = "dotdash"),
                        
                        labels = c("communist votes in all communities", "Communits votes in the communities with high level of emigration the the East", "Communits votes in the communities with high level of emigration the the West")) +
  
  scale_shape_manual(labels = c("communist votes in all communities", "Communits votes in the communities with high level of emigration the the East", "Communits votes in the communities with high level of emigration the the West")) +
  
  scale_colour_manual(values= c("all_comvot"="black" , "east_comvot"="red", "west_comvot" = "blue"),
                      labels = c("communist votes in all communities", "Communits votes in the communities with high level of emigration the the East", "Communits votes in the communities with high level of emigration the the West")) +
  
  guides(linetype = guide_legend(nrow = 3),
         shape = guide_legend(nrow = 3),
         color = guide_legend(nrow = 3)) +
  
  labs(linetype = NULL, shape = NULL, colour = NULL,) +
  
  
  # label financial crisis
  geom_vline(xintercept = 1998.8) +
  annotate("text", x = 1998.9, y = 60, label= "Russian financial cirsis ~ begin of emigration", hjust = 0, size = 8) +
  
  # label na 
  annotate("text", x = 1998, y = 2, label= "n.a.", size = 8) +
  
  # label calls
  annotate("text", x = 2008, y = 39, label = "received calls", size = 6, color = "#FFA500") +
  annotate("text", x = 2002.7, y = 20, label = "emigrants", size = 6, color = "#D3D3D3") +
  
  theme_classic(base_size = 22) +
  
  theme(legend.position = "bottom") 


ggsave(paste0(OUTPUT, "replication_fig_1.png"), width = 40, height = 25, units = "cm")
unlink(fig_1)



################################################################################
################################################################################
#FIGURE 2: OBSERVED SPATIAL PATTERNS OF EMIGRATION FROM MOLDOVA: OVERALL MIGRATION PREVALENCE AND SHARE OF WESTWARD MIGRATION ACROSS COMMUNITIES
################################################################################
################################################################################

# migrants_calls <- read_dta(paste0(INPUT, "mda2_data.dta"))

# aoi_boundary_HARV <- st_read(paste0(INPUT, "Moldova.shp"))




################################################################################
################################################################################
# FIGURE 4: EMIGRATION IN 2004 AND SHARE OF COMMUNIST VOTES IN JULY 2009 ACROSS COMMUNITIES
################################################################################
################################################################################
load(paste0(INPUT, "community_data.rda"))

scatterplot_comvot <- function(level, comvot_election){
  
  if(level == "prev_mig_all"){
    plot_title = "Overall migration"
    x_title = "Overall prevalence of emigration (%)"
    png_name = "fig_4_1_all"
  } else if (level == "prev_mig_west") {
    plot_title = "Westward migration"
    x_title = "Prevalence of emigration to the West (%)"
    png_name = "fig_4_2_west"
  } else if (level == "prev_mig_east") {
    plot_title = "Eastward migration"
    x_title = "Prevalence of emigration to the East (%)"
    png_name = "fig_4_3_east"
  }
  
  if(comvot_election == "comvot21jul"){
    lab_year = "July 2021"
  } else if(comvot_election == "comvot14nov"){
    lab_year = "November 2014"
  } else if(comvot_election == "comvot09jul"){
    lab_year = "July 2009"
  }
  
  figure <- ggplot(community_data, aes(y = get(comvot_election), x = get(level)), colour = "red") +
    geom_point(shape = 18) +
    geom_smooth(method = lm, se = FALSE) +
    
    
    # axis settings
    scale_x_continuous(limits = c(0, 20), breaks = c(0, 5, 10, 15, 20), name = x_title) +
    
    scale_y_continuous(limits = c(0,100), breaks = c(0, 20, 40, 60, 80, 100), name = paste0("Communist votes (%) ", lab_year)) +
    
    # title
    labs(title = plot_title) + 
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  ggsave(paste0(OUTPUT, png_name, "_", comvot_election,".png"), width = 15, height = 15, units = "cm")
  unlink(png_name)
  
}

### 2021
# Overall migration
scatterplot_comvot("prev_mig_all", "comvot21jul")
# Migration to the west
scatterplot_comvot("prev_mig_west", "comvot21jul")
# Migration to the east
scatterplot_comvot("prev_mig_east", "comvot21jul")

### 2014
# Overall migration
scatterplot_comvot("prev_mig_all", "comvot14nov")
# Migration to the west
scatterplot_comvot("prev_mig_west", "comvot14nov")
# Migration to the east
scatterplot_comvot("prev_mig_east", "comvot14nov")

### 2009

# Overall migration
scatterplot_comvot("prev_mig_all", "comvot09jul")
# Migration to the west
scatterplot_comvot("prev_mig_west", "comvot09jul")
# Migration to the east
scatterplot_comvot("prev_mig_east", "comvot09jul")


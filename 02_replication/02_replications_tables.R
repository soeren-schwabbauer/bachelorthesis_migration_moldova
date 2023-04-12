rm(list = ls())
################################################################################
#Project:       migration_moldova
#Author:        Sören Schwabbauer
#Initial_Date:  11.04.23
#Purpose:       replicate results from "The Effect of Labor Migration on the 
#               Diffusion of Democracy: Evidence from a Former Soviet Republic"
#               with the new election results as explanatory variable
################################################################################
################################################################################

# load libraries
pacman::p_load(dplyr, foreign, lmtest, sandwich, estimatr)

# define input, output
INPUT = paste0(getwd(), "/01_generate_data/OUTPUT/")
#OUTPUT = paste0(getwd(), "/01_generate_data/OUTPUT/") 


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
# TABLE 1: MIGRATION PATTERNS AND RESULTS OF THE JULY 2009 PARLIAMENTARY ELECTION (INCLUDES TABLE A4)
################################################################################
################################################################################

# Basic controls
model1 <- paste("comvot09jul ~ prev_mig_west + prev_mig_east +", 
                paste(controls, collapse = "+"),
                "+ as.factor(cod_rai)")

model1 <- lm_robust(as.formula(model1), data = community_data, clusters = cod_rai, se_type = "stata")
tab1_1 <- summary(model1)
tab1_1


# Plus pre-migration election results
model2 <- paste("comvot09jul ~ prev_mig_west + prev_mig_east +", 
                paste(controls, collapse = "+"), "+",
                paste(pre_treatment1994, collapse = "+"), "+", 
                paste(pre_treatment1998, collapse = "+"),
                "+ as.factor(cod_rai)")
model2 <- lm_robust(as.formula(model2), data = community_data, clusters = cod_rai, se_type = "stata")
tab1_2 <- summary(model2)
tab1_2


# Plus night-time light (baseline specification)
model3 <- paste("comvot09jul ~ prev_mig_west + prev_mig_east +", 
                paste(controls, collapse = "+"), "+",
                paste(pre_treatment1994, collapse = "+"), "+", 
                paste(pre_treatment1998, collapse = "+"), "+",
                paste(lights),
                "+ as.factor(cod_rai)")
model3 <- lm_robust(as.formula(model3), data = community_data, clusters = cod_rai, se_type = "stata")
tab1_3 <- summary(model3)
tab1_3


# Heterogeneity within the West
model4 <- paste("comvot09jul ~  prev_mig_west_full + prev_mig_west_flaw + prev_mig_east +", 
                paste(controls, collapse = "+"), "+",
                paste(pre_treatment1994, collapse = "+"), "+", 
                paste(pre_treatment1998, collapse = "+"), "+",
                paste(lights),
                "+ as.factor(cod_rai)")
model4 <- lm_robust(as.formula(model4), data = community_data, clusters = cod_rai, se_type = "stata")
tab1_4 <- summary(model4)
tab1_4

### share of votes (Column 5-8)
table_1_5_8 <- function(shareofvotes){

model <- paste(shareofvotes, " ~  prev_mig_west + prev_mig_east +", 
                paste(controls, collapse = "+"), "+",
                paste(pre_treatment1994, collapse = "+"), "+", 
                paste(pre_treatment1998, collapse = "+"), "+",
                paste(lights),
                "+ as.factor(cod_rai)")
model <- lm_robust(as.formula(model), data = community_data, clusters = cod_rai, se_type = "stata")
tab1_5_8 <- summary(model)
tab1_5_8

}

# Share of votes for Liberal Democratic Party
table1_5_8("ldvot09jul")

# Share of votes for Liberal Party
table1_5_8("libvot09jul")

# Share of votes for Democratic Party
table1_5_8("pdmvot09jul")

# Share of votes for Party Alliance Our Moldova
table1_5_8("amnvot09jul")


################################################################################
################################################################################
# TABLE 2: MIGRATION PATTERNS AND COMMUNIST VOTES OVER TIME, 2001–2010 
################################################################################
################################################################################

table_2 <- function(comvotyear){

model1 <- paste(comvotyear, " ~  prev_mig_west + prev_mig_east +", 
                paste(controls, collapse = "+"), "+",
                paste(pre_treatment1994, collapse = "+"), "+", 
                paste(pre_treatment1998, collapse = "+"), "+",
                paste(lights),
                "+ as.factor(cod_rai)")
model1 <- lm_robust(as.formula(model1), data = community_data, clusters = cod_rai, se_type = "stata")
tab2 <- summary(model1)
tab2

}

# Communist votes 2001
table_2("comvot01")

# Communist votes 2005
table_2("comvot05")

# Communist votes April 2009
table_2("comvot09apr")

# Communist votes July 2009
table_2("comvot09jul")

# Communist votes 2010
table_2("comvot10")


## communist mayors

# Communist mayor 1999
model6 <- paste("commay99 ~  prev_mig_west + prev_mig_east +", 
                paste(controls, collapse = "+"), "+",
                paste(pre_treatment1994, collapse = "+"), "+", 
                "+ lights92_98 + as.factor(cod_rai)")
model6 <- lm_robust(as.formula(model6), data = community_data, clusters = cod_rai, se_type = "stata")
tab2_6 <- summary(model6)
table2_6

# Communist mayor 2003
table_2("commay03")

# Communist mayor 2007
table_2("commay07")



################################################################################
################################################################################
# TABLE 3: MIGRATION PATTERNS AND COMMUNIST VOTES OVER TIME, 2001–2010 
################################################################################
################################################################################

# estimate propensity score
community_data$comvotavg <- rowMeans(community_data[, c("comvot01", "comvot05", "comvot09apr", "comvot09jul", "comvot10")])

model1 <- paste("comvotavg ~ ",
                paste(controls2, collapse = "+"), "+",
                paste(pre_treatment1994, collapse = "+"), "+", 
                paste(pre_treatment1998, collapse = "+"), "+",
                paste(lights), 
                "+ as.factor(cod_rai)")
model1 <- lm_robust(as.formula(model1), data = community_data, clusters = cod_rai, se_type = "stata")
tab3_1 <- summary(model1)
tab3_1

community_data$pscore_avg <- predict(model1, newdata = community_data)

### Communist votes
table_3 <- function(year){

model <- paste(year ,"~ prev_mig_west + prev_mig_east + pscore_avg")
model <- lm_robust(as.formula(model), data = community_data, clusters = cod_rai, se_type = "stata")
tab3_ <- summary(model)
tab3_
}

# Communist votes 2001
table_3("comvot01")

# Communist votes 2005
table_3("comvot05")

# Communist votes April 2009
table_3("comvot09apr")

# Communist Votes July 2009
table_3("comvot09jul")

# Communist Votes April 2010
table_3("comvot10")


################################################################################
################################################################################
# TABLE 4: HETEROGENEITY OF THE EFFECT OF MIGRATION PATTERNS ON COMMUNIST VOTES
################################################################################
################################################################################
data <- community_data %>% filter(urban == 0)
model <- paste("comvot09jul ~ prev_mig_west + prev_mig_east +",
               paste(controls, collapse = "+"), "+",
               paste(pre_treatment1994, collapse = "+"), "+", 
               paste(pre_treatment1998, collapse = "+"), "+",
               paste(lights),
               " + as.factor(cod_rai)")
model <- lm_robust(as.formula(model), data = data, clusters = cod_rai)
tab3_ <- summary(model)




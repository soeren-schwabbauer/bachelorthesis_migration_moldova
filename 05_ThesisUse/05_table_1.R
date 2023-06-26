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
library(dplyr)
library(foreign)
library(lmtest)
library(sandwich) 
library(estimatr) 
library(texreg) 
library(latexpdf) 
library(huxtable)
library(tinytex)
library(stargazer)

# define input, output
INPUT = paste0(getwd(), "/05_ThesisUse/INPUT/")
OUTPUT = paste0(getwd(), "/05_ThesisUse/OUTPUT/") 


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
# TABLE 1: MIGRATION PATTERNS AND RESULTS OF THE  PARLIAMENTARY ELECTION (INCLUDES TABLE A4)
################################################################################
################################################################################

### Basic controls
election_results_col1 <- function(outcome_var){
  model <- paste(paste(outcome_var, " ~ prev_mig_west + prev_mig_east +"),
                 paste(controls, collapse = "+"),
                 "+ as.factor(cod_rai)")
  
  model <- lm_robust(as.formula(model), data = community_data, clusters = cod_rai, se_type = "stata")
}

### Plus pre-migration results
election_results_col2 <- function(outcome_var){
  model <- paste(paste(outcome_var, " ~ prev_mig_west + prev_mig_east +"),
                 paste(controls, collapse = "+"), "+",
                 paste(pre_treatment1994, collapse = "+"), "+", 
                 paste(pre_treatment1998, collapse = "+"),
                 "+ as.factor(cod_rai)")
  
  model <- lm_robust(as.formula(model), data = community_data, clusters = cod_rai, se_type = "stata")
}

### Plus night-time light (baseline specification)
election_results_col3 <- function(outcome_var){
  model <- paste(paste(outcome_var, " ~ prev_mig_west + prev_mig_east +"), 
                 paste(controls, collapse = "+"), "+",
                 paste(pre_treatment1994, collapse = "+"), "+", 
                 paste(pre_treatment1998, collapse = "+"), "+",
                 paste(lights),
                 "+ as.factor(cod_rai)")
  
  model <- lm_robust(as.formula(model), data = community_data, clusters = cod_rai, se_type = "stata")
}

### Heterogeneity within the West
election_results_col4 <- function(outcome_var){
  
  model <- paste(outcome_var, "~  prev_mig_west_full + prev_mig_west_flaw + prev_mig_east +", 
                 paste(controls, collapse = "+"), "+",
                 paste(pre_treatment1994, collapse = "+"), "+", 
                 paste(pre_treatment1998, collapse = "+"), "+",
                 paste(lights),
                 "+ as.factor(cod_rai)")
  model <- lm_robust(as.formula(model), data = community_data, clusters = cod_rai, se_type = "stata")
  
}

election_results_col5_8 <- function(shareofvotes){
  
  model <- paste(shareofvotes, " ~  prev_mig_west + prev_mig_east +", 
                 paste(controls, collapse = "+"), "+",
                 paste(pre_treatment1994, collapse = "+"), "+", 
                 paste(pre_treatment1998, collapse = "+"), "+",
                 paste(lights),
                 "+ as.factor(cod_rai)")
  model <- lm_robust(as.formula(model), data = community_data, clusters = cod_rai, se_type = "stata")
}

# communist votes 2021
table1 <- huxreg("Basic controls" = election_results_col1("comvot09jul"),
                 "Plus pre- \n migration election \n results" = election_results_col2("comvot09jul"),
                 "Plus \n nighttime \n light (full \n model)" = election_results_col3("comvot09jul"),
                 "Hetero- geneity \n within \n the West" = election_results_col4("comvot09jul"),
                 
                 "Liberal\nDemocratic \n party" = election_results_col5_8("ldvot09jul"),
                 "Liberal Party" = election_results_col5_8("libvot09jul"),
                 "Demo- \n cratic Party" = election_results_col5_8("amnvot09jul"),
                 "Party \n Alliance\n Our \n Moldova" = election_results_col5_8("pdmvot09jul"),
                 
                 
                 
                 coefs = c("Prevalence of emigration to the West (percent)" = "prev_mig_west",
                           "Prevalence of emigration to the East (percent)" = "prev_mig_east",
                           "Prevalence of emigration to flawed Western democracies (percent)" = "prev_mig_west_flaw",
                           "Prevalence of emigration to full Western democracies (percent)" = "prev_mig_west_full"),
                 stars = NULL,
                 align = "r",
                 error_pos = "same", 
                 number_format = 2) %>%
  
  ## for subcaption
  insert_row("", "Share of votes for \n the Communist parties (percent)", "", "", "", "Share of votes for \n opposition parties (percent)", "", "", "", after = 0) %>%
  
  
  merge_cells(1, 2:5) %>%
  set_bottom_border(1, 2:5) %>%
  
  merge_cells(1, 6:9) %>%
  set_bottom_border(1, 6:9) %>%
  
  set_bottom_border(2, 0:9) %>%
  
  
  set_align(everywhere, everywhere, "center") 
  
#  set_caption("Migration Patterns and Results of the July 2009 Parliamentary Election") 

#table1 <- add_footnote(table1, "Notes: The table reports OLS estimates for 848 Moldovan communities. The dependent variables are the vote 
#shares of different parties in the July 2009 parliamentary election at the community level (in percent). The set of basic controls includes community-level variables capturing population size, age structure, ethnic composition, skill level, and distribution of the population, a dummy for district capitals and the cities of Chisinau and Balti, the distance to the district capital, and the next Romanian border crossing. Table A4 in the online Appendix shows the full regression results. Standard errors clustered at the district level are in parentheses. Column 4 distinguishes between full and flawed democracies within Western destinations based on the classification provided by the Economist Intelligence Unit’s index of democracy of 2006 (the index is not available for earlier years). Full Western democracies include Portugal, Greece, Spain, France, Germany, the Czech Republic, Great Britain, Ireland, the United States, Belgium, Austria, Canada, Switzerland, and the Netherlands. Flawed Western democracies include Italy, Romania, Israel, Cyprus, Bulgaria, and Poland. Moldova is also classified as a flawed democracy")

##
col_width(table1) <- c(0.3, 0.09, 0.09, 0.09, 0.09, 0.09, 0.08, 0.09, 0.08)
width(table1) <- 1.3
wrap(table1) <- TRUE
font_size(table1) <- 8
position(table1) <- "left"

print_latex(table1)

capture.output(print_latex(ht), file = paste0(OUTPUT, "table1.tex"))
quick_pdf(table1, file =  paste0(OUTPUT, "table1.pdf"))


ht <- huxtable(
  a = 1:3,
  b = letters[1:3]
)
print_latex(ht)

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
library(stargazer)
library(rrtable)
library(psycModel)
library(pagedown)

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



### Basic controls
election_results_col1 <- function(outcome_var){
  
  model <- paste(paste(outcome_var, " ~ prev_mig_west + prev_mig_east +"),
                 paste(controls, collapse = "+"),
                 "+ as.factor(cod_rai)")
  
  lm(as.formula(model), data = community_data)
}

### Plus pre-migration results
election_results_col2 <- function(outcome_var){
  model <- paste(paste(outcome_var, " ~ prev_mig_west + prev_mig_east +"),
                 paste(controls, collapse = "+"), "+",
                 paste(pre_treatment1994, collapse = "+"), "+", 
                 paste(pre_treatment1998, collapse = "+"),
                 "+ as.factor(cod_rai)")
  
  lm(as.formula(model), data = community_data)
}

### Plus night-time light (baseline specification)
election_results_col3 <- function(outcome_var){

    model <- paste(paste(outcome_var, " ~ prev_mig_west + prev_mig_east +"), 
                 paste(controls, collapse = "+"), "+",
                 paste(pre_treatment1994, collapse = "+"), "+", 
                 paste(pre_treatment1998, collapse = "+"), "+",
                 paste(lights),
                 "+ as.factor(cod_rai)")
  
  lm(as.formula(model), data = community_data)
}




stargazer(election_results_col1("comvot09jul"),
          election_results_col2("comvot09jul"),
          election_results_col3("comvot09jul"),
          

          
          keep = c("prev_mig_west", "prev_mig_east", "prev_mig_west_flaw", "prev_mig_west_full"),
          
          
          covariate.labels=c("Prevalence of emigration <br> to the West (percent)", 
                             "Prevalence of emigration <br> to the East (percent)", 
                             "Prevalence of emigration <br> to flawed Western democracies (percent)",
                             "Prevalence of emigration <br> to full Western democracies (percent)"),
          
          dep.var.labels = c("Share of votes for the Communist parties (percent)",
                             "Share of votes for opposition parties (percent)"
                             ),

         column.labels = c("Basic <br> controls","Plus <br> pre-migration election results", "test"),
         model.numbers = F,
         
          
         dep.var.caption = "",
          
          se = starprep(election_results_col1("comvot09jul"),
                        election_results_col2("comvot09jul"),
                        election_results_col3("comvot09jul"),
                        
                        clusters = community_data$cod_rai, se_type = "stata"),
          
          
          omit.stat = c("LL","ser","f"),
          type= "text",
          out = paste0(OUTPUT, "table1.html")
          )




html_to_pdf(file_path = paste0(OUTPUT, "table1.html"), dir = paste0(OUTPUT, "table1.pdf"))

df <- HTMLcode2latex(table)


test <- election_results_col2("comvot09jul")

rm(list = ls())
################################################################################
#Project:       migration_moldova
#Author:        Sören Schwabbauer
#Initial_Date:  11.04.23
#Purpose:       Prepare data with election results
################################################################################
################################################################################

# load libraries
pacman::p_load(readxl, tidyverse)

# define input, output
INPUT = paste0(getwd(), "/01_generate_data/INPUT/")
OUTPUT = paste0(getwd(), "/01_generate_data/OUTPUT/") 


# load df
census_pop_char <- read_excel(paste0(INPUT, "census_2014_population_characteristics.xls"), sheet = "1", skip = 5)


### make df useable
census_pop_char <- census_pop_char %>%
  
  rename(name_prim = '...1',
         pop_size = '...2',
         )

## change rownames


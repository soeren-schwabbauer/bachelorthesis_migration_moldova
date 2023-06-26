rm(list = ls())


# load libraries
library(tidyr)
library(dplyr)
library(haven)
library(data.table)
library(gridExtra)
library(ggplot2)
library(rlang)


### define Input, OUTPUT
INPUT = paste0(getwd(), "/05_ThesisUse/INPUT/")
OUTPUT = paste0(getwd(), "/05_ThesisUse/OUTPUT/")

### load theme
source(paste0(getwd(), "/05_ThesisUse/00_theme.R"))

### load df
load(paste0(INPUT, "community_data.rda"))



scatterplot_comvot <- function(level, comvot_election){
  
  if(level == "prev_mig_all"){
    plot_title = "Overall migration"
    x_title = "Overall prevalence of emigration (%)"
  } else if (level == "prev_mig_west") {
    plot_title = "Westward migration"
    x_title = "Prevalence of emigration to the West (%)"
    png_name = "fig_4_2_west"
  } else if (level == "prev_mig_east") {
    plot_title = "Eastward migration"
    x_title = "Prevalence of emigration to the East (%)"
  }
  
  if(comvot_election == "comvot21jul"){
    lab_year = "July 2021"
  } else if(comvot_election == "comvot14nov"){
    lab_year = "November 2014"
  } else if(comvot_election == "comvot09jul"){
    lab_year = "July 2009"
  }
  
  figure <- ggplot(community_data, aes(y = get(comvot_election), x = get(level)), colour = "red") +
    geom_point(shape = 18, size = 3, color = "#D3D3D3") +
    geom_smooth(method = lm, se = FALSE, color = "black") +
    
    
    # axis settings
    scale_x_continuous(limits = c(0, 20), breaks = c(0, 5, 10, 15, 20), name = x_title) +
    
    scale_y_continuous(limits = c(0,100), breaks = c(0, 20, 40, 60, 80, 100), name = paste0("Communist votes (%) ", lab_year)) +
    
     
    theme_bachelor
  
  

  
}

### 2021

# Overall migration
ggsave(paste0(OUTPUT, "fig_cor_mig_comvot_overall.pdf"), width = 35.55556, height = 20,units = "cm", device=cairo_pdf)
print(scatterplot_comvot("prev_mig_all", "comvot21jul"))
dev.off()


# Migration to the west
ggsave(paste0(OUTPUT, "fig_cor_mig_comvot_west.pdf"), width = 35.55556, height = 20,units = "cm", device=cairo_pdf)
print(scatterplot_comvot("prev_mig_west", "comvot21jul"))
dev.off()

# Migration to the east
ggsave(paste0(OUTPUT, "fig_cor_mig_comvot_east.pdf"), width = 35.55556, height = 20,units = "cm", device=cairo_pdf)
print(scatterplot_comvot("prev_mig_east", "comvot21jul"))
dev.off

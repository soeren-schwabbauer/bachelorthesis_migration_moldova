rm(list = ls())

library(dplyr)
library(ggplot2)
library(showtext)
library(extrafont)
library(grDevices)

#font_import(paths = "C:/Windows/Fonts", pattern = "Latin Modern*")
#font_import(pattern = "lmroman*") 

#loadfonts()



INPUT = paste0(getwd(), "/05_ThesisUse/INPUT/")
OUTPUT = paste0(getwd(), "/05_ThesisUse/OUTPUT/")

### load theme
source(paste0(getwd(), "/05_ThesisUse/00_theme.R"))

### load df
load(paste0(INPUT, "gov_coalitions.rda")) 

df_gov <- df_gov %>% mutate(foreign_pol = as.factor(foreign_pol))



##### plot for government coalitions


gov_coaltion <- ggplot() +
  
  geom_col(aes(x = df_gov$year_char, y = df_gov$gov_seats, fill = df_gov$foreign_pol), width= 0.3) +
  
  scale_fill_manual(values = c("Independent" = "lightgrey",
                                "Moldovenism" = "#E58304",
                                "Pro-Romania" = "darkgrey",
                                "pro-European" = "blue",
                                "pro-Russian" = "red")) +
  
  
  # geom_point(aes(x = df_foreign$year, y = df_foreign$seats, color = df_foreign$foreign_pol), size = 3) +
  # geom_line(aes(x = df_foreign$year, y = df_foreign$seats, color = df_foreign$foreign_pol), size = 2) +
  
  
  geom_hline(yintercept = 1/2*101) +
  geom_hline(yintercept = 3/5*101) +
  
  # scale_x_discrete(breaks = as.character(c(seq(1990, 2022, 3)))) +
  scale_y_continuous(breaks = c(seq(0, 80, 10))) +
  
  labs(#title = "Composition of government coaltion by direction of foreign-policy",
    y = "Number of seats",
    x = "",
    fill = "Foreign Policy")  +
  
 
  theme_bachelor +
  
  theme(axis.title.x = element_blank()) 

gov_coaltion


### save file
ggsave(paste0(OUTPUT, "gov_coalition.pdf"), width = 35.55556, height = 20, units = "cm", device=cairo_pdf)
print(gov_coaltion)
dev.off()


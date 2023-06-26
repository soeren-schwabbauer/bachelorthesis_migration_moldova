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
load(paste0(INPUT, "elections_comvot.rda"))
migrants_calls <- read_dta(paste0(INPUT, "migrants_calls.dta"))



##### build 
fig_comvot <- ggplot2::ggplot(comvot) +
  
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
  annotate("text", x = 1998.9, y = 60, label= "Russian financial cirsis ~ begin of emigration", hjust = 0, size = 6, family = "LM Roman 10") +
  
  # label na 
  annotate("text", x = 1998, y = 2, label= "n.a.", size = 8, family = "LM Roman 10") +
  
  # label calls
  annotate("text", x = 2007, y = 39, label = "Calls from abroad", size = 6, family = "LM Roman 10") +
  annotate("text", x = 2002, y = 20, label = "Emigrants (bars)", size = 6,  family = "LM Roman 10") +

  theme_bachelor  



### save file
ggsave(paste0(OUTPUT, "comvot.pdf"), width = 35.55556, height = 20, units = "cm", device=cairo_pdf)
print(fig_comvot)
dev.off()


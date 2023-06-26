rm(list = ls())

library(readxl)
library(dplyr)


INPUT = paste0(getwd(), "/04_DataBuild/INPUT/")
OUTPUT = paste0(getwd(), "/04_DataBuild/OUTPUT/")

df <- read_excel(paste0(INPUT, "election_results.xlsx"))

df %>% 
  group_by(year, foreign_pol) %>%
  summarise(seats = sum(seats)) %>% 
  
  ggplot() +
  
  geom_point(aes(x = year, y = seats, shape = foreign_pol, color = foreign_pol)) +
  
  geom_line(aes(x = year, y = seats, color = foreign_pol)) +
  
  geom_hline(yintercept = 51) +
  
  geom_hline(yintercept = 3/5*100) +
  
  theme_minimal(base_size = 22)


df_gov <- df %>% filter(power == "government") %>%
 # filter(year <= 2001) %>%
  
  group_by(year, year_char, foreign_pol) %>%
  summarise(gov_seats = sum(seats)) %>%
  
  arrange(desc(year))



df_foreign <- df %>% filter(power == "government") %>%
  filter(year >= 2001) %>%
  group_by(year, foreign_pol) %>%
  summarise(seats = sum(seats)) 


### save file

save(df_gov, file = paste0(OUTPUT, "gov_coalitions.rda"))

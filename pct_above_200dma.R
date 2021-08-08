# Libraries ----

library(tidyquant)
library(tidyverse)
library(ggtext)
library(plotly)
library(timetk)
library(janitor)
library(gganimate)
library(lubridate)
library(readxl)


end <- Sys.Date()
data_path <- "~/R/asx/data/"   # path to the data


# References ----
# Can read directly from here: http://www.asx.com.au/asx/research/ASXListedCompanies.csv
# I have done my own mapping for GICS sectors.
# see here: https://us.spindices.com/documents/index-policies/2018-gics-changes.xlsx?force_download=true
#### Even better here: https://www.asx300list.com/ ###

# Looks like the above site doesn't do a download anymore. Better to scrape table, then to a left-join 
# on the ASX csv (first reference above) for gathering sector information.

# Visualisation Inspiration ---- 
# https://alphaarchitect.com/2020/01/23/visualization-sector-trends-with-r-code/


# READ DATA ----
valid_stocks <- read_excel(str_c(data_path,"2021-08-05-asx300.xlsx")) %>%
  select(Code, Company, Sector) %>%  
  mutate(Code = str_c(Code,".AX")) %>% 
  filter(!is.na(Sector))  

# Data Wrangling ----
# Clean variable (column) names
valid_stocks <- valid_stocks %>% clean_names() %>% glimpse()

asx300 <- tq_get(valid_stocks,
                     get  = "stock.prices",
                     from = end - (6 * 365),
                     to   = end)


# Above 50-day MA ----

# * Overall ----
pct_above_sma50 <- asx300 %>% 
  
  arrange(date) %>%
  group_by(code) %>% 
  mutate(sma_50 = slidify_vec(
    .x = adjusted,
    .period = 50,  
    .f = ~ mean(., na.rm = TRUE),
    .align = "right")
  ) %>%
  mutate(
    sma_50_above_adj = case_when(sma_50 > adjusted ~ 1,
                                 TRUE              ~ 0)) %>% 
  ungroup() %>%
  drop_na() %>% 
  group_by(date) %>% 
  count(sma_50_above_adj) %>%  
  mutate(percent = n/sum(n),
         percent_label = scales::percent(percent)) %>% 
  ungroup() %>% 
  filter(sma_50_above_adj == 1)


pct_above_sma50 %>%
  filter(!is.na(percent)) %>% 
  ggplot(aes(date, percent)) +
  geom_line(aes(), alpha = 0.65, size = 0.85) +
  # geom_smooth(method = 'lm') +
  
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(min(pct_above_sma50$date), max(pct_above_sma50$date) + 30)) +
  
  labs(x = "Date", y = "Percent above 50-day SMA",
       title = "Percentage of ASX300 stocks above 50-day moving average",
       caption = "Dataviz: @GrantChalmers | Source: Yahoo Finance") +
  
  # scale_colour_manual(values = wesanderson::wes_palette("Cavalcanti1")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.subtitle = element_text(size = 8),
        legend.position = "none",
        plot.background = element_rect(fill = 'antiquewhite', colour = 'antiquewhite'),
        panel.background = element_rect(fill = 'snow'),
        plot.caption = element_text(size = 8, color = "gray50", face = "italic"))

ggsave("asx300_above_50dsma.png", path = "images", width = 8, height = 6)


# * By sector
sector_pct_above_sma50 <- asx300 %>% 
  
  arrange(date) %>%
  group_by(code) %>% 
  mutate(sma_50 = slidify_vec(
    .x = adjusted,
    .period = 50,  
    .f = ~ mean(., na.rm = TRUE),
    .align = "right")
  ) %>%
  mutate(
    sma_50_above_adj = case_when(sma_50 > adjusted ~ 1,
                                 TRUE              ~ 0)) %>% 
  ungroup() %>%
  drop_na() %>% 
  group_by(date, sector) %>% 
  count(sma_50_above_adj) %>%  
  mutate(percent = n/sum(n),
         percent_label = scales::percent(percent)) %>% 
  ungroup() %>% 
  # Remove small cohorts
  filter(sma_50_above_adj == 1,
         !sector %in% c("Class Pend", "Household & Personal Products", "Not Applic",
                        "Consumer Durables & Apparel", "Technology Hardware & Equipment",
                        "Pharmaceuticals, Biotechnology & Life Sciences", "Automobiles & Components"))


sector_pct_above_sma50 %>%
  filter(!is.na(percent)) %>% 
  ggplot(aes(date, percent)) +
  geom_line(aes(), alpha = 0.65, size = 0.85) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(min(sector_pct_above_sma50$date), max(sector_pct_above_sma50$date) + 30)) +
  facet_wrap(~ sector, ncol = 3) +
  
  labs(x = "Date", y = "Percent above 50-day SMA",
       title = "Percentage of ASX300 stocks above 50-day moving average",
       caption = "Dataviz: @GrantChalmers | Source: Yahoo Finance") +
  
  # scale_colour_manual(values = wesanderson::wes_palette("Cavalcanti1")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.subtitle = element_text(size = 8),
        legend.position = "none",
        plot.background = element_rect(fill = 'antiquewhite', colour = 'antiquewhite'),
        panel.background = element_rect(fill = 'snow'),
        plot.caption = element_text(size = 8, color = "gray50", face = "italic"))

ggsave("asx300_sector_above_50dsma.png", path = "images", width = 9, height = 9)

# Above 200-day MA ----
# Getting some weird errors below

pct_above_sma200 <- asx300 %>% 
  
  arrange(date) %>% 
  group_by(code) %>%
  filter(!is.na(adjusted)) %>% 
  mutate(sma_200 = slidify_vec(
    .x = adjusted,
    .period = 200,  
    .f = ~ mean(., na.rm = TRUE),
    .align = "right")
  ) %>%
  mutate(
    sma_200_above_adj = case_when(sma_200 > adjusted ~ 1,
                                 TRUE              ~ 0)) %>% 
  ungroup() %>%
  drop_na() %>% 
  group_by(date, sector) %>% 
  count(sma_200_above_adj) %>%  
  mutate(percent = n/sum(n),
         percent_label = scales::percent(percent)) %>% 
  ungroup() %>% 
  # Remove small cohorts
  filter(sma_200_above_adj == 1,
         !sector %in% c("Class Pend", "Household & Personal Products", "Not Applic",
                        "Consumer Durables & Apparel", "Technology Hardware & Equipment",
                        "Pharmaceuticals, Biotechnology & Life Sciences", "Automobiles & Components"))


pct_above_sma200 %>%
  
  ggplot(aes(date, percent)) +
  geom_line(aes(), alpha = 0.65, size = 1) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(min(pct_above_sma200$date), max(pct_above_sma200$date) + 30)) +
  facet_wrap(~ sector, ncol = 3) +
  
  labs(x = "Date", y = "Percent above 200-day SMA",
       title = "Percentage of ASX300 stocks above 200-day moving average",
       caption = "Dataviz: @GrantChalmers | Source: Yahoo Finance") +
  
  # scale_colour_manual(values = wesanderson::wes_palette("Cavalcanti1")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.subtitle = element_text(size = 8),
        legend.position = "none",
        plot.background = element_rect(fill = 'antiquewhite', colour = 'antiquewhite'),
        panel.background = element_rect(fill = 'snow'),
        plot.caption = element_text(size = 8, color = "gray50", face = "italic"))

ggsave("asx300_above_200dsma.png", path = "images", width = 10, height = 6)  

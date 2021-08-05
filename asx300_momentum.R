# ASX data
# Load libraries ----
library(tidyquant)
library(tidyverse)
library(ggtext)
library(plotly)
library(roll)
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
valid_stocks <- read_excel(str_c(data_path,"20200501-asx300.xlsx"), skip = 1) %>%
  select(Code, Company, Sector, `Market Cap`, `Weight(%)`) %>%  
  mutate(Code = str_c(Code,".AX")) %>% 
  filter(!is.na(Sector))  

# Data Wrangling ----
# Clean variable (column) names
valid_stocks <- valid_stocks %>% clean_names() %>% glimpse()

# Convert weight percent to decimal
asx300 <- valid_stocks %>% 
  select(code, company, sector)

# View count of groups (sectors)
asx300 %>% group_by(sector) %>% 
  summarise(n = n()) %>% 
  head(12)

# Test first!!
# asx300_sample <- asx300 %>% sample_n(10)
# 
# asx_prices <- tq_get(asx300_sample,
#                      get  = "stock.prices",
#                      from = end - 180,
#                      to   = end)
# Perform the real run - do we really need 3 years?

# Read in ASX prices via tidyquant ----
asx_prices <- tq_get(asx300,
                     get  = "stock.prices",
                     from = end - (3 * 365),
                     to   = end)

# Further Wrangling ----
sma50_sma_200_most_recent <- asx_prices %>% 
  group_by(code) %>% 
  mutate(sma_50 = roll_mean(as.matrix(close), 50, complete_obs = T),
         sma_200 = roll_mean(as.matrix(close), 200, complete_obs = T),
         sma_50_greater_than_sma_200 = case_when(sma_50 > sma_200 ~ 1, 
                                                 TRUE ~ 0)) %>% 
  na.omit() %>% 
  filter(date == max(date)) %>% 
  group_by(sector) %>% 
  count(sma_50_greater_than_sma_200) %>%  
  mutate(percent = n/sum(n), 
         trend = case_when(sma_50_greater_than_sma_200 == 1 ~ "sma50 above sma200", 
                           TRUE ~ "sma50 below sma200"),
         percent_label = scales::percent(percent)) %>%
  select(sector, trend, percent, percent_label) %>% 
  group_by(trend) %>% 
  # coerce sector labels to wrap text so they'll fit on our chart
  mutate(sector = str_wrap(sector, width = 13))

# Visualisation ----
# ggplot
sma50_sma_200_most_recent %>% 
  ggplot(aes(x = sector, y = percent, fill = trend, text = str_glue("{sector}: {percent_label}"))) + 
  geom_col(width = .3, position = position_dodge(width = .5), show.legend = F) +
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "", 
       y = "", 
       title = paste("ASX 300 percent SMA 50 <span style='color:darkgreen'>above</span> or <span style='color:darkred'>below</span> SMA 200 \n as of", today()),
       caption = "Source: @GrantChalmers | Yahoo Finance | https://www.asx300list.com/") +
  scale_fill_manual(values = c( "darkgreen", "darkred")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_markdown(hjust = .5),
        plot.caption = element_text(size = 9,color = "gray50", face = "italic"),
        plot.subtitle = element_text(hjust = .5))

# plotly ----

(sma50_sma_200_most_recent %>% 
    ggplot(aes(x = sector, y = percent, fill = trend, text = str_glue("{sector}: {percent_label}"))) + 
    geom_col(width = .3, position = position_dodge(width = .5), show.legend = F) +
    scale_y_continuous(labels = scales::percent_format(), 
                       breaks = scales::pretty_breaks(n = 10)) +
    labs(x = "", 
         y = "", 
         title = paste("ASX 300 percent SMA 50 <span style='color:darkgreen'>above</span> or <span style='color:darkred'>below</span> SMA 200 \n as of", today()),
         caption = "Source: @GrantChalmers | Yahoo Finance | https://www.asx300list.com/") +
    scale_fill_manual(values = c( "darkgreen", "darkred")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          plot.title = element_markdown(hjust = .5),
          plot.caption = element_text(size = 9,color = "gray50", face = "italic"),
          plot.subtitle = element_text(hjust = .5),
          legend.position = "none")) %>% 
  ggplotly(tooltip = "text")

# gganimate ----
sma50_sma_200_for_gif <- asx_prices %>% 
  group_by(code) %>% 
  mutate(sma_50 = roll_mean(as.matrix(close), 50, complete_obs = T),
         sma_200 = roll_mean(as.matrix(close), 200, complete_obs = T),
         sma_50_greater_than_sma_200 = case_when(sma_50 > sma_200 ~ 1, 
                                                 TRUE ~ 0)) %>% 
  na.omit() %>% 
  filter(date > "2019-01-01") %>% 
  group_by(sector, date) %>% 
  count(sma_50_greater_than_sma_200) %>% 
  mutate(percent = n/sum(n), 
         trend = case_when(sma_50_greater_than_sma_200 == 1 ~ "sma50 above sma200", 
                           TRUE ~ "sma50 below sma200"),
         percent_label = scales::percent(percent)) %>%
  group_by(trend) %>% 
  mutate(sector = str_wrap(sector, width = 13)) %>%
  group_by(strftime(date, "%Y-%m")) %>% #Groups by the yearmonths
  filter(date == max(date)) %>% 
  ungroup()

p <- sma50_sma_200_for_gif %>% 
  ggplot(aes(x = reorder(factor(sector), desc(factor(sector))), y = percent, fill = trend)) + 
  geom_col(width = .3, position = position_dodge(width = .5), show.legend = F) +
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "ASX 300 percent SMA 50 <span style='color:forestgreen'>above</span> or <span style='color:firebrick'>below</span> SMA 200",
       subtitle = " {closest_state}",
       y= "",
       x = "",
       caption = "Source: @GrantChalmers | Yahoo Finance | https://www.asx300list.com/") +
  coord_flip() +
  scale_fill_manual(values = c("forestgreen", "firebrick")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.85, size = 10),
        axis.text.y = element_text(angle = 0, hjust = 0.85, size = 10),
        plot.title = element_markdown(hjust = .5),
        plot.caption = element_text(size = 9, colour = "gray50", face = "italic"),
        plot.subtitle = element_text(hjust = .5)) +
  transition_states(date,
                    transition_length = 5,
                    state_length = 5) +
  # some aesthetic bells and whistles to make stuff prettier
  ease_aes('cubic-in-out') +
  enter_fade() +
  exit_shrink()
# Animate
animate(p, fps = 5, renderer = gifski_renderer(loop = TRUE), duration = 30)

# save last animation
anim_save("asx300_sma_comparison.gif", animation = last_animation(), path = "gifs")


# Setup custom colours 
my_colours <- c("Industrials"             = "#56B4E9",
                "Consumer\nStaples"       = "#009E73",
                "Consumer\nDiscretionary" = "gold4",
                "Financials"              = "#CC79A7",
                "Energy"                  = "peru",
                "Materials"               = "peachpuff4",
                "Real Estate"             = "forestgreen",
                "Utilities"               = "#686868",
                "Communication\nServices" = "salmon",
                "Health Care"             = "midnightblue",
                "Information\nTechnology" = "darkorchid4")

# Check colours
scales::show_col(my_colours)

# geom_tile ggplot ----
# geom_tile percentage sma 50 below sma 200 - static
sma50_sma_200_most_recent %>% 
  filter(trend == "sma50 above sma200") %>%
  mutate(ordering = rank(percent, ties.method = "random"),
         percent_label = scales::percent(round(percent, 2))) %>% 
  ggplot(aes(ordering, group = sector, color = sector, fill = sector)) +
  geom_tile(aes(y = percent/2, 
                height = percent ,
                width = .9), alpha = 0.9) +
  # text on top of bars
  geom_text(aes(y = percent, label =  sector ), hjust = -0.1) +
  geom_text(aes(y = percent, label =  percent_label ), color = "white", hjust = 1.2) +
  # text in x-axis (requires clip = "off" in coord_cartesian)
  coord_flip(clip = "off", expand = T)   +
  expand_limits(y = c(.1, 1.2)) +
  scale_fill_manual(values = my_colours) +
  scale_colour_manual(values = my_colours) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "", y = "", title = "Percentage of ASX 300 stocks with a SMA 50 above SMA 200", 
       subtitle = paste("as of", today() - 3),
       caption = "Source: @GrantChalmers | Yahoo Finance | https://www.asx300list.com/") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        plot.caption = element_text(size = 8, colour = "gray50", face = "italic"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

dev.off()
ggsave("asx300_sma50_above_sma200.png", path = "images", width = 8, height = 6)

# Animated version of above:
# geom_tile percentage sma 50 above sma 200 - animated version, create tibble.

sma50_sma_200_by_mth <- asx_prices %>% 
  group_by(code) %>% 
  mutate(sma_50 = roll_mean(as.matrix(close), 50, complete_obs = T),
         sma_200 = roll_mean(as.matrix(close), 200, complete_obs = T),
         sma_50_greater_than_sma_200 = case_when(sma_50 > sma_200 ~ 1, 
                                                 TRUE ~ 0)) %>% 
  na.omit() %>% 
  group_by(sector, date) %>% 
  count(sma_50_greater_than_sma_200) %>%  
  mutate(percent = n/sum(n), 
         trend = case_when(sma_50_greater_than_sma_200 == 1 ~ "sma50 above sma200", 
                           TRUE ~ "sma50 below sma200"),
         percent_label = scales::percent(percent)) %>% 
  group_by(trend) %>%
  filter(sma_50_greater_than_sma_200 == 1) %>%
  mutate(date = ymd(date)) %>%
  group_by(date) %>%
  mutate(ordering = rank(percent, ties.method = "random"),
         percent_label = scales::percent(round(percent, 2))) %>%
  group_by(strftime(date, "%Y-%m")) %>% #Groups by the yearmonths
  filter(date == max(date)) %>%
  # coerce sector labels to wrap text so they'll fit on our chart
  mutate(sector = str_wrap(sector, width = 13)) %>% 
  ungroup()

# Write to rds file
sma50_sma_200_by_mth %>% 
  write_rds("data/sma50_sma_200_by_mth.rds")

# read rds file
sma50_sma_200_by_mth <- read_rds("data/sma50_sma_200_by_mth.rds")

# geom_tile ggplot, gganimate version ----
rank_plot_ani <- sma50_sma_200_by_mth %>% 
  filter(date >= "2021-01-01") %>% 
  ggplot(aes(ordering, group = sector, color = sector, fill = sector)) +
  geom_tile(aes(y = percent/2, 
                height = percent ,
                width = .9), alpha = 0.9) +
  # text on top of bars
  geom_text(aes(y = percent, label =  sector ), hjust = -0.1) +
  geom_text(aes(y = percent, label =  percent_label ), color = "white", hjust = 1.2) +
  # text in x-axis (requires clip = "off" in coord_cartesian)
  coord_flip(clip = "off", expand = T)   +
  expand_limits(y = c(.1, 1.2)) +
  scale_fill_manual(values = my_colours) +
  scale_colour_manual(values = my_colours) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), labels=scales::percent) +
  # guides(color=F,fill=F) +
  labs(x = "", y = "", title = "Percentage of ASX 300 stocks with a SMA 50 above SMA 200", 
       subtitle = paste("as of", today() - 3),
       caption = "Source: @GrantChalmers | Yahoo Finance | https://www.asx300list.com/") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = .5),
        legend.position = "none",
        plot.subtitle = element_text(size = 12, colour = "darkgreen", hjust = .5),
        plot.caption = element_text(size = 9, colour = "gray50", face = "italic"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  transition_states(date, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out') +
  labs(x = "", y = "", title = "Percentage of ASX 300 stocks with a SMA 50 above SMA 200", 
       subtitle = "as of {closest_state}",
       caption = "Source: @GrantChalmers | Yahoo Finance | https://www.asx300list.com/"
  ) 

animate(rank_plot_ani, fps = 10, duration = 30)

anim_save("rank_plot_10fps.gif", animation = last_animation(), path = "gifs")

sma50_sma_200_by_mth %>% 
  filter(date == min(date))



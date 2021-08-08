# LIBRARIES ----
library(tidyverse)
library(tidyquant)
library(janitor)
library(readxl)

start <- "2015-01-01"
end <- Sys.Date()
data_path <- "~/R/asx/data/"   # path to the data

# References: ASX300 list https://www.asx300list.com/ ####
# Try to replicate this slightly outdate code: https://github.com/mdancho84/analysis_Russell2000
# Associated blog: https://www.business-science.io/investments/2016/11/30/Russell2000_Analysis.html

# READ DATA ----
valid_stocks <- read_excel(str_c(data_path,"20200401-asx300.xlsx"), skip = 1) %>%
  select(Code, Company, Sector, `Market Cap`, `Weight(%)`) %>%  
  mutate(Code = str_c(Code,".AX")) %>% 
  filter(!is.na(Sector))  # some blanks may need to be filled in manually in Excel. Check in Commsec.

# Clean variable (column) names
valid_stocks <- valid_stocks %>% clean_names() %>% glimpse()


# Test first!!
# asx300_sample <- valid_stocks %>% sample_n(10)

# asx_prices <- tq_get(asx300_sample,
#                      get  = "stock.prices",
#                      from = start,
#                      to   = today())

asx300_prices <- tq_get(valid_stocks,
                 get  = "stock.prices",
                 from = start,
                 to   = today())

# WRANGLING ----

# Remove country code
asx300_prices$code <- gsub("\\..*", "", asx300_prices$code)

asx300_prices

asx300_prices %>% write_rds(str_c(data_path, "asx300_prices.rds"))
# asx300_prices <- read_rds(str_c(data_path, "asx300_prices.rds"))


asx300 <- asx300_prices %>%
  # Include company for labeling later
  group_by(code, company) %>%
  filter(!is.na(adjusted)) %>% 
  tq_transmute(adjusted, 
               periodReturn, 
               period = "daily", 
               type = "log", #log or arithmetic?? not sure..
               col_rename = "log_returns") %>%
  mutate(mean_log_returns = mean(log_returns),
         sd_log_returns = sd(log_returns)) %>%
  add_tally(name = "trade_days") %>% 
  ungroup()

asx300

# VISUALISATION ----

trade_day_thresh <- 1400
sd_limit <- 0.075

asx300 %>% 
  filter(trade_days >= trade_day_thresh,
         sd_log_returns <= sd_limit) %>%
  ggplot(aes(x = sd_log_returns, y = mean_log_returns)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth() +
  theme_tq() +
  scale_colour_tq() +
  labs(title = "Market Tends to Penalise Stocks with Large SDDLR",
       subtitle = "Best to Focus on Stocks with Highest Ratio of MDLR to SDDLR",
       x = "Standard Deviation of Daily Log Returns (SDDLR)",
       y = "Mean Daily Log Returns (MDLR)")

# Add new reward-to-risk metric (reward_metric) to our tibble
asx300_tbl <- asx300 %>%
  filter(trade_days >= 1200,
         !code == "ISX") %>% #date > end - years(5)
  mutate(reward_metric = 2500 * (mean_log_returns / sd_log_returns),
         year = year(date),
         text_col = ifelse(reward_metric < 100, "red", "black")) %>%
  group_by(code, year) %>%
  mutate(avg_annual_log_returns = mean(log_returns)) %>% 
  ungroup()

asx300_tbl

# * Static version ----
g <- asx300_tbl %>% 
  filter(trade_days >= trade_day_thresh,
         sd_log_returns <= sd_limit) %>%
  mutate(label_text = str_glue("Stock: {code}
                               Company: {company}
                               No of Trading Days: {trade_days}
                               Reward to Risk: {round(reward_metric, 1)}")) %>%
  ggplot(aes(x = sd_log_returns, y = mean_log_returns,
             colour = reward_metric, text = label_text)) +
  geom_point(aes(size = reward_metric), alpha = 0.5) +
  theme_tq() +
  viridis::scale_color_viridis() +
  guides(size = FALSE) +
  labs(title = "ASX 300 Analysis: Stock Risk vs Reward",
       x = "Risk: Standard Deviation of Daily Log Returns (SDDLR)",
       y = "Reward: Mean Daily Log Returns (MDLR)",
       colour = "Reward Metric")

g

# * Interactive Version ----
library(plotly)
g %>%
  ggplotly(tooltip = "text")

# * By Sector ----
asx300_tbl %>% 
  left_join(asx300_prices %>% distinct(code, sector)) %>%  
  filter(trade_days >= trade_day_thresh,
         sd_log_returns <= sd_limit, 
         year == 2020) %>% # for testing as dataset is pretty big
  ggplot(aes(x = sd_log_returns, y = mean_log_returns,
             colour = sector)) +
  geom_point(size = 3) +
  theme_tq() +
  scale_colour_tq(theme = "dark") +
  # Adjust point size in legend to match plot
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  labs(title = "ASX 300 Analysis: Stock Risk vs Reward by Sector in 2020",
       x = "Risk: Standard Deviation of Daily Log Returns (SDDLR)",
       y = "Reward: Mean Daily Log Returns (MDLR)",
       colour = "Sector")

ggsave("asx300_risk_v_reward_by_sector.png", path = "images", width = 7, height = 5)

# * Static - geom_text_repel ----
# Try geom_text_repel as an option because plotly is so memory taxing

# test filter to make sure there are not too many to plot legibly
asx300_tbl %>% filter(date == max(date), reward_metric >= 100 | reward_metric < -30) %>% view()
  
asx300_tbl %>%
  filter(trade_days >= trade_day_thresh,
         sd_log_returns <= sd_limit) %>%
  ggplot(aes(x = sd_log_returns, y = mean_log_returns,
             colour = reward_metric, label = code)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(size = 1.25), alpha = 0.5) +
  ggrepel::geom_text_repel(
    data          = asx300_tbl %>% filter(date == max(date), reward_metric >= 100 | reward_metric <= -30),
    force         = 1,
    colour        = ifelse(asx300_tbl %>% 
                             filter(date == max(date),
                                    reward_metric >= 100 | reward_metric <= -30) %>% 
                             select(text_col) == "red", "red", "darkgreen"),
    size          = 2,
    segment.color = "grey40",
    segment.size  = 0.2, 
    direction     = "both",
    arrow         = arrow(length = unit(0.012, "npc")),
    box.padding   = 0.225 
  ) +
  theme_tq() +
  viridis::scale_color_viridis() +
  guides(size = FALSE) +
  labs(title = "ASX 300 Analysis: Stock Risk vs Reward",
       x = "Risk: Standard Deviation of Daily Log Returns (SDDLR)",
       y = "Reward: Mean Daily Log Returns (MDLR)",
       colour = "Reward Metric")

ggsave("asx300_risk_v_reward_top_labeled_red_darkgreen.png", path = "images", width = 7, height = 5)


# Part 2: ----
# Visualise Top 15 Stocks to Understand Consistent Growth ----

# This took ages to get right.
top_24 <- asx300_tbl %>%
  arrange(code, desc(reward_metric)) %>%
  group_by(code) %>% 
  mutate(rank = row_number()) %>% 
  arrange(rank, desc(reward_metric)) %>% 
  ungroup() %>% 
  slice(1:24) %>% 
  pull(code)

top_24

top_24_by_year <- asx300_tbl %>% 
  filter(code %in% top_24) %>% 
  distinct(code, year, avg_annual_log_returns)

top_24_by_year

# Visualise

top_24_by_year %>%
  ggplot(aes(x = year, y = avg_annual_log_returns)) +
  geom_line(aes(col = code)) +
  geom_hline(yintercept = 0, col = "salmon") +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ code, nrow = 4, ncol = 6) +
  theme_tq() +
  theme(legend.position = "None", axis.text.x = element_text(angle=90)) +
  labs(title = "Best Prospects Have Consistent, Above-Zero MDLR and Growth",
       subtitle = "Trend Flat to Upward Indicates Growth",
       x = "Year",
       y = "Mean Daily Log Returns (MDLR)")

# Attribute 1 ----
# Number of Times MDLR by Year Drops Below Zero
means_below_zero_tbl <- top_24_by_year %>%
  mutate(count_means_below_zero = if_else(avg_annual_log_returns < 0, 1, 0)) %>% 
  group_by(code) %>% 
  summarise(means_below_zero = sum(count_means_below_zero)) %>% 
  ungroup()

means_below_zero_tbl

# Attribute 2 ----
# Slope of MDLR by Year
slope_tbl <- top_24_by_year %>% 
  group_by(code) %>% 
  summarise(broom::tidy(lm(avg_annual_log_returns ~ year))) %>% 
  filter(term == "year") %>% 
  select(code, slope = estimate)
  
slope_tbl

# Attribute 3 ----
# Standard deviation of MDLR by year
sd_of_means_by_year_tbl <- top_24_by_year %>% 
  group_by(code) %>% 
  summarise(sd_of_means_by_year = sd(avg_annual_log_returns)) %>% 
  ungroup()

sd_of_means_by_year_tbl

# Join attribute tibbles from above into one tibble
attributes_tbl <- means_below_zero_tbl %>%
  left_join(slope_tbl) %>% 
  left_join(sd_of_means_by_year_tbl)

attributes_tbl

# Create a growth metric variable and arrange by it
attributes_tbl <- attributes_tbl %>% 
  mutate(growth_metric = slope / ((means_below_zero + 1) * sd_of_means_by_year)) %>% 
  select(code, growth_metric, slope, means_below_zero, sd_of_means_by_year) %>% 
  arrange(desc(growth_metric))

attributes_tbl #%>% write_csv("asx300_top_stocks.csv")

# Gather top 12 stocks for filtering asx300_prices tibble
top_12 <- attributes_tbl %>%
  slice(1:12) %>% 
  pull(code)

top_12

# Visualise Top 12 ----

asx300_prices %>%
  filter(code %in% top_12) %>% 
  ggplot(aes(x = date, y = adjusted, colour = code)) +
  geom_line() +
  facet_wrap(~ company, nrow = 3, scales = "free_y") +
  theme_tq() +
  scale_colour_tq() +
  theme(legend.position = "None",
        strip.text.x = element_text(size = 7, colour = "skyblue"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  labs(title = "Prospecting Best ASX 300 Stocks",
       subtitle = "Twelve Stocks with Amazing Growth, Most Consistency",
       x = "Year",
       y = "Price per Share")

ggsave("top_12_growth_consistency.png", path = "images", width = 8, height = 6)

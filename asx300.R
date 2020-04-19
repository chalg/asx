# ASX data
library(tidyverse)
library(tidyquant)
library(janitor)

end <- Sys.Date()
data_path <- "~/R/asx/data/"   # path to the data

# Original inspiration: https://www.michaelplazzer.com/import-asx-data-r/
# Read in data from csv
# Can read directly from here: http://www.asx.com.au/asx/research/ASXListedCompanies.csv
# I have done my own mapping for GICS sectors.
# see here: https://us.spindices.com/documents/index-policies/2018-gics-changes.xlsx?force_download=true
#### Even better here: https://www.asx300list.com/ ####

valid_stocks <- read_csv(paste0(data_path,"20200401-asx300.csv"), skip = 1) %>%
  select(Code, Company, Sector, `Market Cap`, `Weight(%)`) %>%  
  mutate(Code = paste0(Code,".AX")) %>% 
  filter(!is.na(Sector))       # could filter out in Excel

# Clean variable (column) names
valid_stocks <- valid_stocks %>% clean_names() %>% glimpse()

# Convert weight percent to decimal
asx300 <- valid_stocks %>% 
  mutate(weights = weight_percent / 100) %>% 
  select(code, weights, everything())

# View count of groups (sectors)
asx300 %>% group_by(sector) %>% 
  summarise(n = n()) %>% 
  head(12)

# Test first!!
asx300_sample <- asx300 %>% sample_n(10)

asx_prices <- tq_get(asx300_sample,
                      get  = "stock.prices",
                      from = end - 180,
                      to   = end)
# Perform the real run
asx_prices <- tq_get(asx300,
                     get  = "stock.prices",
                     from = end - 365,
                     to   = end)

# Reference. Ignore the below for now because you can read in the csvs below.
# https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ05-performance-analysis-with-tidyquant.html#portfolios-asset-groups
stock_returns_monthly <- asx_prices %>%
  group_by(code, sector) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")
stock_returns_monthly

# asx_prices %>% write_csv(paste0(data_path, "asx_prices.csv"))
# stock_returns_monthly %>% write_csv(paste0(data_path, "stock_returns_monthly.csv"))

# read these in (not required in final script...)
stock_returns_monthly <- read_csv(paste0(data_path, "stock_returns_monthly.csv"))
asx_prices <- read_csv(paste0(data_path, "asx_prices.csv"))

# Add weights back in as second column (as per documentation)
stock_returns_monthly <- stock_returns_monthly %>%
  left_join(asx_prices) %>% 
  select(sector, wts = weights, code, date, Ra)

glimpse(stock_returns_monthly)

# Calculate sector weights
sector_weights <- stock_returns_monthly %>%
  distinct(code, wts, sector) %>% 
  group_by(sector) %>% 
  summarise(sect_wts_total = sum(wts))

glimpse(sector_weights)
# Add sector weights column
stock_returns_monthly <- stock_returns_monthly %>% left_join(sector_weights)
glimpse(stock_returns_monthly)

# Create weights table
weights_tbl <- stock_returns_monthly %>% 
  group_by(code, sector) %>% 
  mutate(sector_wts = wts / sect_wts_total) %>%
  ungroup() %>% 
  select(sector, sector_wts)
glimpse(weights_tbl)

#### Not sure how to make this work per sector?? ####
mult_monthly_returns_stocks <- tq_repeat_df(stock_returns_monthly, n = 11)
glimpse(mult_monthly_returns_stocks)

# Error: weights must be grouped by portfolio index.
pfolio <- tq_portfolio(data = mult_monthly_returns_stocks,
                       assets_col = sector,
                       returns_col = Ra,
                       weights = weights_tbl,
                       col_rename = NULL,
                       wealth.index = FALSE)

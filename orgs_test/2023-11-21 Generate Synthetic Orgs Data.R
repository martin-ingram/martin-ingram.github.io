## Synthetic Landscape Data
# 2023-11-21 | Martin Ingram

library(tidyverse)
library(tidylog)

# List of fictional organisations
orgs_list <- 
    readxl::read_xlsx("orgs_test/2023-11-21 fictional_orgs_list.xlsx")

# years of data we want to generate
years <- 2010:2023

# Set random seed or reproducibility
set.seed(123) 

# build time series data
org_time_series <- replicate(n = length(years), orgs_list, 
                            simplify = FALSE) %>% 
    data.table::rbindlist() %>% 
    arrange(organisation) %>% 
    mutate(time = rep(years, 45) , .before = 1)

# clean content and add numeric columns
output_org_time_series <- org_time_series %>% 
    mutate(content_type = sub(x = source, 
                              pattern = " - .*",
                              replacement = ""), 
           source_content = sub(x = source, 
                                pattern = ".* - ",
                                replacement = ""),
           .before = source) %>% 
    select(-source) %>% 
    mutate(across(c("organisation",
                    "content_type",
                    "source_content"),
                  \(x) str_remove_all(x, 
                                      pattern = ",|'|’"))) %>% 
    mutate(content_type = recode(content_type, 
                "Comic/Video Game" = "Comic")) %>% 
    mutate(fte = sample(400:1000,
                        nrow(org_time_series),
                        replace = TRUE)) %>% 
    mutate(running_costs = fte * sample(250:35000,
                    nrow(org_time_series), replace = TRUE)) %>% 
    mutate(investment_costs = running_costs * 0.1046) %>% 
    mutate(funding = running_costs + investment_costs) %>% 
# Add noise and randomness
    mutate(investment_costs =
               ifelse(runif(nrow(org_time_series)) > 0.9,
                      investment_costs * 
                    sample(30:55, nrow(org_time_series),
                           replace = TRUE),investment_costs)) %>% 
    mutate(noise = rnorm(n = nrow(org_time_series),
                         mean = mean(funding),
                         sd = sd(funding)) / 10) %>% 
    mutate(funding = funding + noise) %>% 
    select(-noise)

write_csv(output_org_time_series,
          file = "orgs_test/2023-11-21 - fictional_orgs_time_series.csv")
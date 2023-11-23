## Synthetic Landscape Data
# 2023-11-21 | Martin Ingram

library(tidyverse)
library(tidylog)

# Set random seed or reproducibility
set.seed(123) 

# List of fictional organisations
orgs_list <- 
    readxl::read_xlsx("orgs_test/2023-11-21 fictional_orgs_list.xlsx") %>% 
    mutate(fte = rnorm(n = nrow(.), mean = 1500, sd = 1050)) %>% 
    mutate(fte = case_when(.default = fte, fte < 0 ~ 0))

# years of data we want to generate
years <- 2010:2023

# build time series data
org_time_series <- replicate(n = length(years), orgs_list, 
                            simplify = FALSE) %>% 
    data.table::rbindlist() %>% 
    arrange(organisation) %>% 
    mutate(time = rep(years, 44) , .before = 1)

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
    mutate(running_costs = running_costs + noise * 4) %>% 
    mutate(funding = funding + noise) %>% 
    mutate(noise_fte = rnorm(n = nrow(org_time_series),
                         mean = mean(fte),
                         sd = sd(fte)) / 20) %>% 
    group_by(organisation) %>% 
    mutate(noise_fte = cumsum(noise_fte)) %>% 
    mutate(fte = case_when(.default = fte + noise_fte, 
            grepl(x = organisation,
                 pattern = "^F|^H")  ~ fte + (noise_fte * 2),
            grepl(x = organisation,
                  pattern = "^W")  ~ fte - noise_fte,
            grepl(x = organisation,
                  pattern = "^L")  ~ fte - (noise_fte*2),
            time %in% c('2016', "2020") & fte > 2000 & 
                grepl(x = organisation,
                      pattern = "^A|^S|^M|^C|^O")  ~ fte - noise_fte *
                               runif(n = 1, min = 0.04, max = 2))) %>% 
    mutate(fte = case_when(.default = fte,
            fte < 0 ~ 0)) %>% 
    # mutate(fte = ) %>% 
    select(-starts_with('noise')) %>%
    mutate(across(c("fte", 
                    "running_costs",
                    "investment_costs",
                    "funding"), \(x) round(x)))

write_csv(output_org_time_series,
          file = "orgs_test/slides/2023-11-21 - fictional_orgs_time_series.csv")

write_csv(output_org_time_series,
          file = "orgs_test/report/2023-11-21 - fictional_orgs_time_series.csv")

message(paste0(Sys.time(), " - synthetic data rebuilt"))

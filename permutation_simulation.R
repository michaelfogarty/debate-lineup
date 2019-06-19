rm(list=ls())

library(tidyverse)  ## general data manipulation
library(rvest)      ## web scraping
library(randomizr)  ## block random assignment
library(knitr)
library(kableExtra)
library(assertthat)

## Read in data -- scrape from 538

#Specifying the url for desired website to be scraped
url <- 'https://fivethirtyeight.com/features/the-dnc-tried-to-avoid-a-lopsided-debate-it-got-one-anyway/'

#Reading the HTML code from the website
webpage <- read_html(url)

scrape <- 
  webpage %>% 
  html_nodes('#post-213083 > div > section > table') %>% 
  html_table() %>% 
  magrittr::extract2(1) %>% 
  janitor::clean_names() %>% 
  slice(1:10) %>% 
  mutate(avg = as.numeric(str_remove(avg, '%')),
         avg_2 = as.numeric(str_remove(avg_2, '%')))

night_1 <- 
  scrape %>% select(june_26_debate, no_of_polls, avg) %>% 
  mutate(night = 'June 26') %>% 
  rename(candidate = june_26_debate)

night_2 <- 
  scrape %>% select(june_27_debate, no_of_polls, avg_2) %>% 
  mutate(night = 'June 27') %>% 
  rename(candidate = june_27_debate, avg = avg_2)

debates <- 
  bind_rows(night_1, night_2) %>% 
  mutate(night = as_factor(night),
         tier = as_factor(ifelse(avg >= 2, 'A', 'B')))

debates <- as_tibble(debates)

rm(night_1, night_2, scrape, webpage, url)

write_rds(debates, 'debates.RDS')

## simulations

permutation <- function(data) {
  
  assert_that(is_tibble(data))
  
  data$permute <- NA
  
  data$permute <- 
    randomizr::complete_ra(N=20, m=10, 
                           conditions = c('June 26', 'June 27'))
  
  data %>% group_by(permute) %>% summarize(total = sum(avg)) %>% 
    mutate(diff = abs(total - lag(total))) %>% 
    filter(!is.na(diff)) %>% select(diff) %>% magrittr::extract2(1)
  
}

block_permutation <- function(data) {
  
  assert_that(is_tibble(data))
  
  data$permute <- NA
  
  data$permute <- 
    randomizr::block_ra(blocks = debates$tier, 
                        conditions = c('June 26', 'June 27'))
  
  data %>% group_by(permute) %>% summarize(total = sum(avg)) %>% 
    mutate(diff = abs(total - lag(total))) %>% 
    filter(!is.na(diff)) %>% select(diff) %>% magrittr::extract2(1)
  
}


set.seed(1)

n_sim <- 10000

simulations <- 
  n_sim %>% rerun(permutation(debates)) %>% unlist(use.names = FALSE)

simulations_block <- 
  n_sim %>% rerun(block_permutation(debates)) %>% unlist(use.names = FALSE)

simulated_data <- tibble(dist = simulations,
                         block_dist = simulations_block)

write_rds(simulated_data, 'permutation_simulation_results.RDS')

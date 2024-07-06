
# Load libs ----
library(tidyr)
library(stringr)
library(ggplot2)
library(ggsci)
library(scales)
library(dplyr)
library(gcamdata)
library(purrr)
library(patchwork)
library(RColorBrewer)

source("R/LoadPackagesFuncs.R")
source("R/GCAM_module_funcs.R")

DIR_DATA <- "data"
DIR_OUTPUT <- "output"
DIR_MODULE = "AgMIP"



Project <- "AgMIP"

ListJuly2024 <- readRDS(file.path(DIR_OUTPUT, Project, "ProjectRDS", paste0("ListJuly2024", ".RDS")))

readr::read_csv("data/inst/AgMIP_Reporting.csv") -> AgMIP_Reporting




Scen_AgMIP <- AgMIP_Reporting %>% distinct(Scenario) %>% filter(!is.na(Scenario)) %>% pull

PluckBind <- function(.query){
  ListJuly2024 %>% purrr::pluck(.query) %>%
    select(-ss) %>% filter(year %in% 2015:2050) %>%
    mutate(scenario = factor(scenario,
                             levels =  c(AgMIP_Reporting %>% distinct(Scenario) %>%
                                           filter(!is.na(Scenario)) %>% pull)))
}

ProcReg <- function(.df){
  .df %>%
    rename(region0 = region) %>%
    left_join_error_no_match(Regmapping %>% select(region0 = region, region = AgMIP13),
                             by = "region0")
}






# check others ----
ListJuly2024 %>%
  purrr::pluck("MeanTemp") %>%
  select(-ss) %>% filter(year %in% 2015:2050) %>%
  mutate(scenario = factor(scenario,
                           levels = c(AgMIP_Reporting %>% distinct(Scenario) %>%
                                        filter(!is.na(Scenario)) %>% pull))) %>%
  filter(year == 2050) %>%
  ggplot() +
  geom_point(aes(x = scenario, y = value, fill = scenario), shape = 21, size = 4) +
  labs(x = "AgMIP Scenario", y = "°C") +
  theme_bw() + theme0 + theme(legend.position = "none")

ListJuly2024 %>%
  purrr::pluck("ForcingTotal") %>%
  filter(year == 2050) %>%
  ggplot() +
  geom_point(aes(x = scenario, y = value, fill = scenario), shape = 21, size = 4) +
  labs(x = "AgMIP Scenario", y = "W/m2") +
  theme_bw() + theme0 + theme(legend.position = "none")






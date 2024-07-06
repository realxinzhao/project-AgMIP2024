# Load data ----

# There some diff. between this one and the CMP version
# 1. yield bugfix
# 2. regional biomass parameters


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


# Version Sensitivity ----
Version <- "July2024"
Scenario <- Load_GCAM(projnm = Project, versionnm = Version, return_availscen = T)


# Check availability
Load_GCAM(projnm = Project, return_availversion = T)
Load_GCAM(projnm = Project, versionnm = Version, return_availscen = T)
Load_GCAM(projnm = Project, versionnm = Version, return_availquery = T)


rm(ListJuly2024)
## Load everything into lists ----
Load_GCAM(projnm = Project, versionnm = "July2024", outputlistnm = "ListJuly2024")

# create a project data output folder and save data
dir.create(file.path(DIR_OUTPUT, Project, "ProjectRDS"), showWarnings = F)
ListJuly2024 %>% saveRDS(file.path(DIR_OUTPUT, Project, "ProjectRDS", paste0("ListJuly2024", ".RDS")))


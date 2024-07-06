
theme1 <- theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
                strip.background = element_rect(fill="grey99"),
                strip.text = element_text(size = 12),
                axis.text.x.bottom = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                panel.spacing.y = unit(0.5, "lines"),
                panel.spacing.x = unit(0.8, "lines"))


PluckBind <- function(.query){
  ListJuly2024 %>% purrr::pluck(.query) %>%
    select(-ss) %>% filter(year %in% 2015:2050) %>%
    mutate(scenario = factor(scenario,
                             levels =  c(AgMIP_Reporting %>% distinct(Scenario) %>%
                                           filter(!is.na(Scenario)) %>% pull))) %>%
    rename(region0 = region) %>%
    left_join_error_no_match(Regmapping %>% select(region0 = region, region = AgMIP13),
                             by = "region0")
}


## Read in GWP mapping ----
readr::read_csv("data/maps/GWP.csv") -> GWP


# CTAX ----
ListJuly2024 %>% purrr::pluck("CO2Price")  %>%
  select(-ss) %>% filter(market == "USACO2") %>%
  filter(year %in% 2015:2050) %>%
  transmute(Scenario = scenario, Region = "WLD",
            Variable = "CTAX", Item = "TOT",
            Value = value * 12 /44 * gdp_deflator(2015, 1990),
            Year = year,
            Unit = "2015 USD/tCO2e") ->
  CTAX

# Add other scenarios as zeros

CTAX %>%
  distinct(Region, Variable, Item, Year, Unit) %>%
  repeat_add_columns(AgMIP_Reporting %>% distinct(Scenario) %>% filter(!is.na(Scenario)) ) %>%
  left_join(CTAX, by = c("Region", "Variable", "Item", "Year", "Unit", "Scenario")) %>%
  replace_na(list(Value = 0)) %>%
  mutate(Scenario = factor(Scenario,
                           levels =  c(AgMIP_Reporting %>% distinct(Scenario) %>%
                                         filter(!is.na(Scenario)) %>% pull))) ->
  CTAX

# ***CTAX ----
Add_To_AgMIP_Report(CTAX)


# ECH4 EN2O ECO2 NCEM ----

"NCEM" %>% PluckBind() %>%
  Agg_reg(region, GHG, Units) %>%
  rename(CEM_reg = value) %>%
  left_join("NCEM_sector" %>% PluckBind() %>% Agg_reg(region, GHG, Units),
            by = c("scenario", "region", "GHG", "Units", "year") ) %>%
  mutate(value = CEM_reg - value)  %>% select(-CEM_reg) %>%
  mutate(sector = "Resource production") %>%
  ## bind NCEM_sector
  bind_rows("NCEM_sector" %>% PluckBind() ) %>%
  ## bind CLUC
  bind_rows(
    "CLUC" %>% PluckBind() %>% Agg_reg(region, Units) %>% mutate(sector = "LULUCF", GHG = "CO2")
  ) %>%
  Agg_reg(region, GHG, Units, sector) -> NCEM_All


NCEM_All %>%
  filter(GHG == "CO2") %>%
  Agg_reg(region) %>%
  transmute(Scenario = scenario, Region = region, Year = year,
         Variable = "ECO2", Item = "TOT", Value = value * 44/12, Unit = "MtCO2e") ->
  ECO2

ECO2 %>% AgMIP_AggReg() -> ECO2_All
# ***ECO2 ----
Add_To_AgMIP_Report(ECO2_All)

ECO2 %>%
  group_by_at(vars(-Region, -Value)) %>%
  summarize(Value = sum(Value)) %>%
  rename(year= Year, value = Value) %>%
  mutate(value = value / 1000) %>%
  ggplot() +
  geom_point(aes(x = year, y = value, color = Scenario)) +
  labs(x = "Year", y = "GtCO2e",
       title = "Annual CO2 emissions") +
  theme_bw() + theme0 -> p

p %>% Write_png(.name = "ToTCO2_ann", .DIR_MODULE = DIR_MODULE, h = 7, w = 10, r = 300)

ECO2 %>%
  group_by_at(vars(-Region, -Value)) %>%
  summarize(Value = sum(Value)) %>%
  rename(year= Year, value = Value) %>%
  group_by_at(vars(-year, -value) ) %>%
  Fill_annual(CUMULATIVE = T) %>%
  filter(year == 2050) %>% mutate(value = value / 1000) %>%
  mutate(ss = if_else(value <1000, "Mitigation", "No Policy")) %>%
  ggplot() + facet_wrap(~ss, scales = "free_x") +
  geom_point(aes(x = Scenario, y = value, fill = Scenario), shape = 21, color = "black", size =3) +
  #geom_bar(aes(x = Scenario, y = value, fill = Scenario), color= "black", stat = "identity") +
  coord_flip() +
  labs(x = "Scenario", y = "GtCO2e",
       title = "2020 - 2050 cumulative CO2 emissions") +
  theme_bw() + theme0 + theme1 -> p;p

p %>% Write_png(.name = "ToTCO2", .DIR_MODULE = DIR_MODULE, h = 7, w = 10, r = 300)

## LULUCF ----

NCEM_All %>%
  filter(sector == "LULUCF") %>%
  Agg_reg(region) %>%
  transmute(Scenario = scenario, Region = region, Year = year,
            Variable = "LULUCF", Item = "TOT", Value = value * 44/12, Unit = "MtCO2e") %>%
  group_by_at(vars(-Region, -Value)) %>%
  summarize(Value = sum(Value)) %>%
  rename(year= Year, value = Value) %>%
  group_by_at(vars(-year, -value) ) %>%
  Fill_annual(CUMULATIVE = T) %>%
  filter(year == 2050) %>% mutate(value = value / 1000) %>%
  filter(value < 1000) %>%
  ggplot() +
  geom_point(aes(x = Scenario, y = value, fill = Scenario), shape = 21, color = "black", size =3) +
  #geom_bar(aes(x = Scenario, y = value, fill = Scenario), color= "black", stat = "identity") +
  coord_flip() +
  labs(x = "Scenario", y = "GtCO2e",
       title = "2020 - 2050 cumulative LULUCF CO2 emissions") +
  theme_bw() + theme0 + theme1 -> p;p

p %>% Write_png(.name = "LULUCF", .DIR_MODULE = DIR_MODULE, h = 7, w = 8, r = 300)




# ECH4 EN2O NonCO2 details ----

NCEM_All %>%
  filter(!GHG %in% c("CO2"))%>%
  mutate(GHG = replace(GHG, grepl("SO2_", GHG), "SO2_3")) %>%
  left_join(GWP %>% replace_na(list(AR6all = 0)), by = "GHG") %>%
  mutate(GHG1 = if_else(GHG1 == "Other GHGs", GHG1, GHG)) %>%
  mutate(GHG1 = if_else(sector == "UnmanagedLand", paste0(GHG1, "_UnMGMTLand"), GHG1)) %>%
  group_by(scenario, region, AgCOMM = sector, sector = GHG1, year) %>%
  summarise(GHGnc_AR6 = sum(value * AR6all)/1000,
            GHGnc_AR5 = sum(value * AR5all)/1000,
            GHGnc_AR4 = sum(value * AR4all)/1000, .groups = "drop") %>%
  select(-GHGnc_AR5) %>% rename(value = GHGnc_AR6) %>%
  mutate(value = value * 1000 * 12 / 44) %>% # convert to MTC
  filter(grepl("AGR|AWB", sector)) %>%
  mutate(sector = case_when(
    sector %in% c("CH4_AGR", "CH4_AWB") ~ "ECH4",
    sector %in% c("CH4") ~ "CH4_En",
    sector %in% c("N2O_AGR", "N2O_AWB") ~ "EN2O",
    sector %in% c("N2O") ~ "N2O_En",
    TRUE ~ sector)
  ) %>%
  filter(grepl("Resource", AgCOMM) == F) %>%
  Agg_reg(AgCOMM, sector, region) %>%
  mutate(value = value / 1000 * 44/12) %>%
  mutate(year = as.integer(year)) ->
  pNCEM_sector


pNCEM_sector %>%
  left_join_error_no_match(
    MapAgCOMM %>% distinct(AgCOMM, AgMIP) %>%
      filter(!is.na(AgMIP)), by = "AgCOMM"
  ) %>%
  group_by_at(vars(-AgCOMM, -value)) %>%
  # to Mt CO2e
  summarize(value = sum(value) * 1000, .groups = "drop") %>%
  transmute(Scenario = scenario, Region = region, Year = year,
            Variable = sector, Item = AgMIP, Value = value, Unit = "MtCO2e") ->
  ECH4_EN2O

# fill ECP CH4 as zeros
ECH4_EN2O %>%
  spread(Item, Value, fill = 0) %>%
  gather(Item, Value, -Scenario:-Unit) ->
  ECH4_EN2O

# Add CRP and LSP
ECH4_EN2O %>%
  filter(!Item %in% c("RUM", "NRM","DRY")) %>%
  mutate(Item = "CRP") %>%
  group_by_at(vars(-Value)) %>%
  summarize(Value = sum(Value), .groups = "drop") %>%
  bind_rows(
    ECH4_EN2O %>%
      filter(Item %in% c("RUM", "NRM","DRY")) %>%
      mutate(Item = "LSP") %>%
      group_by_at(vars(-Value)) %>%
      summarize(Value = sum(Value), .groups = "drop")
  ) %>%
  bind_rows(
    ECH4_EN2O
  ) ->
  ECH4_EN2O


ECH4_EN2O %>% mutate(Variable = "EMIS") %>%
  group_by_at(vars(-Value)) %>%
  summarize(Value = sum(Value), .groups = "drop") %>%
  bind_rows(
    ECH4_EN2O
  )->
  EMIS_ECH4_EN2O

EMIS_ECH4_EN2O %>% AgMIP_AggReg() -> EMIS_ECH4_EN2O_All
# ***EMIS_ECH4_EN2O_All ----
Add_To_AgMIP_Report(EMIS_ECH4_EN2O_All)


EMIS_ECH4_EN2O %>% filter(Item %in% c("LSP", "CRP")) %>%
  filter(Variable == "EMIS") %>%
  group_by_at(vars(-Region, -Value)) %>%
  summarize(Value = sum(Value)) %>%
  rename(year= Year, value = Value) %>%
  group_by_at(vars(-year, -value) ) %>%
  Fill_annual(CUMULATIVE = T) %>%
  filter(year == 2050) %>% mutate(value = value / 1000) %>%
  #filter(value < 1000) %>%
  ggplot() + facet_wrap(~Item, scales = "free_x") +
  geom_point(aes(x = Scenario, y = value, fill = Scenario), shape = 21, color = "black", size =3) +
  #geom_bar(aes(x = Scenario, y = value, fill = Scenario), color= "black", stat = "identity") +
  coord_flip() +
  labs(x = "Scenario", y = "GtCO2e",
       title = "2020 - 2050 cumulative NonCO2 emissions \nCrop (CRP) & Livestock (LSP) sectors") +
  theme_bw() + theme0 + theme1 -> p;p

p %>% Write_png(.name = "EMIS", .DIR_MODULE = DIR_MODULE, h = 7, w = 10, r = 300)


# [Todo] Add TOT


NCEM_All %>%
  filter(!GHG %in% c("CO2")) %>%
  Agg_reg(region, GHG) %>%
  mutate(GHG = replace(GHG, grepl("SO2_", GHG), "SO2_3")) %>%
  left_join(GWP %>% replace_na(list(AR6all = 0)) %>%
              select(GHG, AR6all), by = "GHG") %>%
  mutate(GHGnc_AR6 = value * AR6all/1000) %>%
  mutate(value = value * 1000 * 12 / 44) %>% # convert to MTC
  filter(!is.na(value)) %>%
  Agg_reg(region)  %>%
  bind_rows(
    NCEM_All %>%
      filter(GHG %in% c("CO2", "CO2_FUG")) %>%
      filter(!is.na(value)) %>%
      Agg_reg(region)
  ) %>%
  mutate(value = value  * 44/12) -> # MtCO2
  totalGHG



# WATR: Water for irrigation  ----

"Water_Wsector" %>% PluckBind() %>%
  inner_join(
    # crop irrigation
    MapAgCOMM %>% filter(AgCOMM0 == "Crop") %>%
      select(AgMIP, sector = AgCOMM),
    by = "sector"
  ) %>%
  Agg_reg(Region = region, Item = AgMIP) %>%
  transmute(Scenario = scenario, Region, Item, Year = year,
            Variable = "WATR",
            Value = value, Unit = "km3") ->
  WATR

# Adding aggregated All crops
WATR %>%
  mutate(Item = "CRP") %>%
  group_by_at(vars(-Value)) %>%
  summarize(Value = sum(Value), .groups = "drop") %>%
  bind_rows(WATR) ->
  WATR

WATR %>% AgMIP_AggReg() -> WATR_All
# ***WATR_ALL ----
Add_To_AgMIP_Report(WATR_All)


WATR %>% filter(Item == "CRP") %>%
  group_by_at(vars(-Region, -Value)) %>%
  summarize(Value = sum(Value)) %>%
  rename(year= Year, value = Value) %>%
  group_by_at(vars(-year, -value) ) %>%
  Fill_annual(CUMULATIVE = T) %>%
  filter(year == 2050) %>% mutate(value = value / 1000) %>%
  #filter(value < 1000) %>%
  ggplot() + facet_wrap(~Item, scales = "free_x") +
  geom_point(aes(x = Scenario, y = value, fill = Scenario), shape = 21, color = "black", size =3) +
  #geom_bar(aes(x = Scenario, y = value, fill = Scenario), color= "black", stat = "identity") +
  coord_flip() +
  labs(x = "Scenario", y = "1000 km3",
       title = "2020 - 2050 cumulative irrigation water use") +
  theme_bw() + theme0 + theme1 -> p;p

p %>% Write_png(.name = "WATR", .DIR_MODULE = DIR_MODULE, h = 7, w = 8, r = 300)



# FRTN	Fertiliser N  ----

"FertizerUse" %>% PluckBind() %>%
  inner_join(
    # crop irrigation
    MapAgCOMM %>% filter(AgCOMM0 == "Crop") %>%
      select(AgMIP, sector = AgCOMM),
    by = "sector"
  ) %>%
  Agg_reg(Region = region, Item = AgMIP) %>%
  transmute(Scenario = scenario, Region, Item, Year = year,
            Variable = "FRTN",
            Value = value*1000, Unit = "1000 t") ->
  FRTN

# Adding aggregated All crops
FRTN %>%
  mutate(Item = "CRP") %>%
  group_by_at(vars(-Value)) %>%
  summarize(Value = sum(Value), .groups = "drop") %>%
  bind_rows(FRTN) ->
  FRTN

FRTN %>% AgMIP_AggReg() -> FRTN_All
# ***FRTN_All ----
Add_To_AgMIP_Report(FRTN_All)


FRTN %>% filter(Item == "CRP") %>%
  group_by_at(vars(-Region, -Value)) %>%
  summarize(Value = sum(Value)) %>%
  rename(year= Year, value = Value) %>%
  group_by_at(vars(-year, -value) ) %>%
  Fill_annual(CUMULATIVE = T) %>%
  filter(year == 2050) %>% mutate(value = value / 1000) %>%
  #filter(value < 1000) %>%
  ggplot() + facet_wrap(~Item, scales = "free_x") +
  geom_point(aes(x = Scenario, y = value, fill = Scenario), shape = 21, color = "black", size =3) +
  #geom_bar(aes(x = Scenario, y = value, fill = Scenario), color= "black", stat = "identity") +
  coord_flip() +
  labs(x = "Scenario", y = "Mt N",
       title = "2020 - 2050 cumulative fertilizer use") +
  theme_bw() + theme0 + theme1 -> p;p

p %>% Write_png(.name = "FRTN", .DIR_MODULE = DIR_MODULE, h = 7, w = 8, r = 300)


# 17 dataframes

AgMIP_Report %>% length()
AgMIP_Report %>% name()

AgMIP_Report %>% bind_rows %>%
  mutate(Model = "GCAM") %>%
  select(Model,Scenario,Region, Item, Variable, Year, Unit, Value) ->
  AgMIP_Report_Export

AgMIP_Report_Export %>%
  readr::write_csv("output/AgMIP/GCAM_AgMIP_Submission_07052024.csv")





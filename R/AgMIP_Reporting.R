

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

# POP and GDP ----

PluckBind("GDPNS") %>%
  transmute(Scenario = scenario, Region = region, Year = year,
            Value = value * gdp_deflator(2005, 1990) / 1000,
            Variable = "GDPT", Item = "GDPT",
            Unit = "bn USD 2005 MER") %>%
  AgMIP_AggReg ->
  GDPT

Add_To_AgMIP_Report(GDPT)

PluckBind("POP") %>%
  Agg_reg(region) %>%
  transmute(Scenario = scenario, Region = region, Year = year,
            Value = value /1000, Variable = "POPT", Item = "POPT",
            Unit = "million") %>%
  AgMIP_AggReg ->
  POPT
Add_To_AgMIP_Report(POPT)

# ***POP and GDP ----


# AREA_ALL: AREA, ARRF, ARIR ----

readRDS("data/inst/L2012.AgHAtoCL_irr_mgmt.rds") -> L2012.AgHAtoCL_irr_mgmt

PluckBind("Detailedland") %>%
  left_join(
    L2012.AgHAtoCL_irr_mgmt %>%
      rename(region0 = region,
             LandLeaf = AgProductionTechnology) %>%
      select(-year, -AgSupplySubsector, -AgSupplySector),
    by = c("region0", "LandLeaf")
  ) %>%
  replace_na(list(harvests.per.year = 1)) %>%
  # convert cover to area
  mutate(value = value * harvests.per.year) %>%
  select(-harvests.per.year) %>%
  separate(LandLeaf, into = c("LandLeaf", "Basin", "IRR", "MGMT")) %>%
  Agg_reg(region, LandLeaf, IRR) %>%
  mutate(value = value/10 * 1000) %>%   # TkM2 to Tha
  left_join(
    LandMapping %>% select(LandLeaf, land = AgMIP_Cover, area = AgMIP_Area), by = "LandLeaf"
  ) %>% filter(!is.na(area)) %>%
  # AgMIP crop sectors
  Agg_reg(Region = region, Item = area, Variable = IRR) %>%
  mutate(Variable = replace(Variable, Variable == "IRR", "ARIR"),
         Variable = replace(Variable, Variable == "RFD", "ARRF") ) %>%
  transmute(Scenario = scenario, Region, Item, Variable, Year = year, Value = value,
            Unit = "1000 ha") ->
  AREF_ARRF

AREF_ARRF %>%
  mutate(Variable = "AREA") %>%
  group_by_at(vars(-Value)) %>%
  summarize(Value = sum(Value), .groups = "drop") %>%
  bind_rows(AREF_ARRF) ->
  AREA_ALL_1

# Adding aggregated All crops
AREA_ALL_1 %>%
  mutate(Item = "CRP") %>%
  group_by_at(vars(-Value)) %>%
  summarize(Value = sum(Value), .groups = "drop") %>%
  bind_rows(AREA_ALL_1) ->
  AREA_ALL

# *** AREA_ALL ----
AREA_ALL %>% AgMIP_AggReg -> AREA_ALL

Add_To_AgMIP_Report(AREA_ALL)


AREA_ALL %>%
  filter(Region == "WLD") %>% mutate(Value = Value / 1000) %>%
  filter(Variable == "AREA") %>%
  filter(Year %in% c(2015, 2020, 2050)) %>%
  mutate(Year = as.character(Year)) %>%
  ggplot() + facet_wrap(~Item, scales = "free_x") +
  geom_point(aes(x = Scenario, y = Value, fill = Year), shape = 21, color ="black") +
  coord_flip() +
  labs(y = "Mt", x = "Scenario",
       title = "MidCentury changes in area harvested across AgMIP Scenarios")+
  theme_bw() + theme0 + theme1 -> p;p
  p %>% Write_png(.name = "AREA", .DIR_MODULE = DIR_MODULE, h = 10, w = 12, r = 300)



ListJuly2024 %>% purrr::pluck("Detailedland") %>%
  select(-ss) %>% filter(year %in% 2015) %>%
  mutate(scenario = factor(scenario,
                           levels =  c(AgMIP_Reporting %>% distinct(Scenario) %>%
                                         filter(!is.na(Scenario)) %>% pull))) %>%
  left_join(
    L2012.AgHAtoCL_irr_mgmt %>%
      rename(LandLeaf = AgProductionTechnology) %>%
      select(-year, -AgSupplySubsector, -AgSupplySector),
    by = c("region", "LandLeaf")
  ) %>%
  replace_na(list(harvests.per.year = 1)) %>%
  # convert cover to area
  mutate(value = value * harvests.per.year) %>%
  select(-harvests.per.year) %>%
  separate(LandLeaf, into = c("LandLeaf", "Basin", "IRR", "MGMT")) %>%
  Agg_reg(region, LandLeaf, IRR) %>%
  mutate(value = value/10 * 1000) -> LandWeight

# YECC ----

ExPrd_GCAM <- readRDS("data/inst/ExPrd_GCAM.rds") %>%
  select(-GCAM_region_ID) %>%
  filter(year %in% seq(2015, 2050, 5)) %>%
  spread(scenario, value)


ExPrd_GCAM %>% mutate(YECC = (rcp7p0 / nocc - 1) * 100) %>%
  select(region, irr, GCAM_sector, year, YECC ) %>%
  left_join(
    LandWeight %>% filter(scenario == "BAU") %>%
      filter(!LandLeaf %in% c("biomassGrass", "biomassTree")) %>%
      select(region, irr = IRR, GCAM_sector = LandLeaf, weight = value),
    by = c("GCAM_sector", "irr", "region")
  ) %>%
  filter(!is.na(weight)) %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = AgMIP13),
                           by = "region0") %>%
  left_join(
    LandMapping %>% select(GCAM_sector = LandLeaf, area = AgMIP_Area), by = "GCAM_sector"
  ) %>% filter(!is.na(area)) %>%
  group_by(Region = region, Item = area, Year = year) %>%
  summarize(YECC = weighted.mean(YECC, w = weight),
            weight = sum(weight),
            .groups = "drop") %>%
  transmute(
    Region, Item, Variable = "YECC", Year, Value = YECC, Unit = "%", weight
  ) %>% repeat_add_columns(
    AgMIP_Reporting %>% distinct(Scenario) %>%
      filter(!is.na(Scenario))
  ) %>% filter(!grepl("NoCC", Scenario)) %>%
  mutate(Scenario = factor(Scenario,
                         levels =  c(AgMIP_Reporting %>% distinct(Scenario) %>%
                                       filter(!is.na(Scenario)) %>% pull))) ->
  YECC

YECC %>% AgMIP_AggReg(WMean = T) -> YECC_All

Add_To_AgMIP_Report(YECC_All)
# *** YECC_All ----

# LAND (Land Cover): LAND ----

PluckBind("Aggland") %>%
  left_join_error_no_match(
    LandMapping %>% select(LandLeaf, land = AgMIP_Cover), by = "LandLeaf"
  ) %>%
  Agg_reg(region, land) %>%
  mutate(value = value/10 * 1000) %>%   # TkM2 to Tha
  transmute(Scenario = scenario, Region = region, Item = land, Variable = "LAND",
            Year = year, Value = value, Unit = "1000 ha") ->
  LAND

# Aggregate to TOT all land

LAND %>%
  # total land
  mutate(Item = "TOT") %>%
  group_by_at(vars(-Value)) %>%
  summarize(Value = sum(Value), .groups = "drop") %>%
  # Bind ag land cropland + grass
  bind_rows(
    LAND %>%
      filter(Item %in% c("GRS", "CRP")) %>%
      mutate(Item = "AGR") %>%
      group_by_at(vars(-Value)) %>%
      summarize(Value = sum(Value), .groups = "drop")
  ) %>%
  bind_rows(LAND) ->
  LAND_ALL_1

LAND_ALL_1 %>% AgMIP_AggReg() %>%
  # Bind energy crop
  bind_rows(
    AREA_ALL %>% filter(Variable == "AREA", Item == "ECP")
  ) -> LAND_ALL

Add_To_AgMIP_Report(LAND_ALL)
# ***LAND_All ----

LAND_ALL %>%
  filter(!Item %in% c("TOT", "NLD")) %>%
  filter(Region == "WLD") %>% mutate(Value = Value / 1000) %>%
  filter(Year %in% c(2015, 2020, 2050)) %>%
  mutate(Year = as.character(Year)) %>%
  ggplot() + facet_wrap(~Item, scales = "free_x") +
  geom_point(aes(x = Scenario, y = Value, fill = Year), shape = 21, color ="black") +
  coord_flip() +
  labs(y = "Mt", x = "Scenario",
       title = "MidCentury changes in land use across AgMIP Scenarios",
       subtitle = "AGR (Cropland & Pasture), CRP (Cropland), ECP (Energy Crop), For (Forest) \nGRS (Pasture), ONV (Other Natural)" ) +
  theme_bw() + theme0 + theme1 -> p;p

p %>% Write_png(.name = "LAND", .DIR_MODULE = DIR_MODULE, h = 10, w = 12, r = 300)



# YILD YIRF_YRIR ----

# Biomass primary energy production; convert to Mt
# Energy content of biomass, GJ/ton
aglu.BIO_ENERGY_CONTENT_GJT <- 17.5


"Agprod" %>% PluckBind() %>%
  # convert biomass EJ to Mt
  filter(Units == "EJ")  %>%
  mutate(value = value * 10^9 / aglu.BIO_ENERGY_CONTENT_GJT / 1000000)  %>%
  mutate(Units = "Mt") %>%
  bind_rows(
    "Agprod" %>% PluckBind() %>%
      filter(Units == "Mt")
  ) %>%
  separate(technology, into = c("LandLeaf", "Basin", "IRR", "MGMT")) %>%
  Agg_reg(region, LandLeaf, IRR) %>%
  mutate(value = value * 1000) %>%   # Mt to 1000 t
  left_join(
    LandMapping %>% select(LandLeaf, land = AgMIP_Cover, area = AgMIP_Area), by = "LandLeaf"
  ) %>%
  filter(!is.na(area)) %>%
  Agg_reg(Region = region, Item = area, Variable = IRR) %>%
  mutate(Variable = replace(Variable, Variable == "IRR", "PRIR"),
         Variable = replace(Variable, Variable == "RFD", "PRRF") ) %>%
  transmute(Scenario = scenario, Region, Item, Variable, Year = year, Value = value,
            Unit = "1000 t") ->
  PROD_Crop_IRR

PROD_Crop_IRR %>%
  mutate(Variable = "PROD") %>%
  group_by_at(vars(-Value)) %>%
  summarize(Value = sum(Value), .groups = "drop")  %>%
  bind_rows(PROD_Crop_IRR) ->
  PROD_Crop_ALL

# Adding aggregated All crops
PROD_Crop_ALL %>%
  mutate(Item = "CRP") %>%
  group_by_at(vars(-Value)) %>%
  summarize(Value = sum(Value), .groups = "drop") %>%
  bind_rows(PROD_Crop_ALL) ->
  PROD_Crop_ALL

PROD_Crop_ALL %>%
  mutate(Variable = replace(Variable, Variable == "PROD", "AREA"),
         Variable = replace(Variable, Variable == "PRIR", "ARIR"),
         Variable = replace(Variable, Variable == "PRRF", "ARRF")) %>%
  left_join(
    AREA_ALL %>% rename(Area = Value) %>% select(-Unit),
    by = c("Scenario", "Region", "Item", "Variable", "Year")
  ) %>%
  mutate(Value = if_else(Area == 0, 0, Value),
         Value = Value / Area,
         Variable = gsub("AREA", "YILD", Variable),
         Variable = gsub("AR", "YI", Variable),
         Unit = if_else(Item == "ECP", "dm t/ha", "fm t/ha") ) %>%
  replace_na(list(Value = 0)) ->
  YILD

YILD %>% rename(weight = Area) %>% AgMIP_AggReg(WMean = T) %>%
  # zero yield for ECP before 2025
  replace_na(list(Value = 0))->
  YILD_All

Add_To_AgMIP_Report(YILD_All)
# ***YILD_All ----

YILD_All %>%
  filter(Region == "WLD") %>%
  filter(Variable == "YILD") %>%
  filter(Year %in% c(2015, 2020, 2025, 2050)) %>%
  mutate(Year = as.character(Year)) %>%
  ggplot() + facet_wrap(~Item, scales = "free_x") +
  geom_point(aes(x = Scenario, y = Value, fill = Year), shape = 21, color ="black") +
  coord_flip() +
  labs(y = "t/ha", x = "Scenario",
       title = "MidCentury changes in yield across AgMIP Scenarios")+
  theme_bw() + theme0 + theme1 -> p;p
p %>% Write_png(.name = "YILD", .DIR_MODULE = DIR_MODULE, h = 12, w = 12, r = 300)





# !! [ToDO!] YEXO ----

aglu.BIO_ENERGY_CONTENT_GJT <- 17.5

"Agprod" %>% PluckBind() %>%
  filter(scenario == "BAU", sector == "biomass", region0 == "USA") %>%
  separate(LandLeaf, into = c("LandLeaf", "Basin", "IRR", "MGMT"))

PluckBind("Detailedland") %>%
  filter(scenario == "BAU", region0 == "USA") %>%
  separate(LandLeaf, into = c("LandLeaf", "Basin", "IRR", "MGMT"), remove = F) -> B


"Agprod" %>% PluckBind() %>%
  # convert biomass EJ to Mt
  filter(Units == "EJ")  %>%
  mutate(value = value * 10^9 / aglu.BIO_ENERGY_CONTENT_GJT / 1000000)  %>%
  mutate(Units = "Mt") %>%
  bind_rows(
    "Agprod" %>% PluckBind() %>%
      filter(Units == "Mt")
  ) %>%
  filter(scenario == "BAU", sector == "biomass", region0 == "USA") %>%
  left_join(
    PluckBind("Detailedland") %>%
      filter(scenario == "BAU", region0 == "USA") %>%
      select(scenario, region0, technology = LandLeaf, year, Prod = value),
    by = c("scenario", "region0", "technology", "year")
  ) -> A

A %>% mutate(Prod = if_else(value == 0, 0, Prod),
             Yield = Prod / value) %>%
  replace_na(list(Yield = 0)) %>% select(-output, -subsector, -Units) %>%
  group_by(scenario, region0, sector, technology) %>%
  mutate(value = if_else(sector == "biomass", value[year == 2025],
                         value[year == 2015]) ) %>%
  ungroup() %>%
  separate(technology, into = c("LandLeaf", "Basin", "IRR", "MGMT")) %>%
  left_join(
    LandMapping %>% select(LandLeaf, land = AgMIP_Cover, area = AgMIP_Area), by = "LandLeaf"
  ) %>% filter(!is.na(area)) %>%
  group_by(scenario, region, Year = year, Item = area) %>%
  summarize(Yield = weighted.mean(Yield, w = value))



# Quantity SUA_FOOD_FEED_TRADE_OTHR_PROD ----
source("R/AgMIP/AgBalElement_Storage.R")

AgElement %>%
  filter(element %in% c("Food", "Feed", "Production", "Revenue", "Import", "Export")) %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = AgMIP13),
                           by = "region0") %>%
  group_by_at(vars(-region0, -value)) %>%
  summarize(value = sum(value), .groups = "drop") ->
  AgElement_AgMIP1

AgElement_AgMIP1 %>% mutate(sector = tolower(sector)) %>%
  filter(sector != "pasture") %>%
  rename(AgCOMM = sector) %>%
  filter(element %in% c("Food", "Feed", "Production", "Revenue", "Import", "Export")) ->
  AgElement_AgMIP2


# Adjust fodder herb for trade and othermeatfish for prod
# agg to AgMIP sector
AgElement_AgMIP2 %>%
  spread(element, value, fill = 0) %>%
  mutate(Production = if_else(AgCOMM == "othermeatfish", Food, Production)) %>%
  mutate(NETT = Export - Import,
         NETT = if_else(AgCOMM == "fodderherb", Export - Feed, NETT)) %>%
  gather(element, value, -scenario:-region) %>%
  left_join(
    MapAgCOMM %>%
      distinct(AgCOMM = tolower(AgCOMM), AgMIP) %>% mutate(AgCOMM = gsub("_", "", AgCOMM)) %>%
      filter(!is.na(AgMIP)), by = "AgCOMM"
  ) %>%
  group_by_at(vars(-value, -AgCOMM)) %>%
  summarize(value = sum(value), .groups = "drop")->
  AgElement_AgMIP3


AgElement_AgMIP3 %>%
  spread(element, value) %>%
  transmute(Scenario = scenario, Region = region, Year = year, Item = AgMIP,
            EXPO =Export, IMPO = Import, PROD = Production, NETT, FOOD = Food, FEED = Feed,
            CONS = PROD - EXPO,
            OTHR = CONS + IMPO - FOOD - FEED) %>%
  gather(Variable, Value, -Scenario:-Item) %>%
  mutate(Value = Value * 1000, Unit = "1000 t") ->
  AgElement_AgMIP4

# Aggregate LSP per FSH_OAP

# LSP does not include FSH!!
AgElement_AgMIP4 %>%
  filter(Item %in% c("FSH_OAP", "RUM", "NRM", "DRY")) %>%
  mutate(Item = "LSP") %>%
  group_by_at(vars(-Value)) %>%
  summarize(Value = sum(Value), .groups = "drop") %>%
  bind_rows(
    AgElement_AgMIP4 %>% filter(Item != "FSH_OAP")
  ) ->
  QuantitySUA_FOOD_FEED_TRADE_OTHR

# overwrite
AgElement_AgMIP4 %>% AgMIP_AggReg() ->
  QuantitySUA_FOOD_FEED_TRADE_OTHR

Add_To_AgMIP_Report(QuantitySUA_FOOD_FEED_TRADE_OTHR)
# **QuantitySUA_FOOD_FEED_TRADE_OTHR_PROD ----

QuantitySUA_FOOD_FEED_TRADE_OTHR %>% distinct(Variable)

QuantitySUA_FOOD_FEED_TRADE_OTHR %>%
  filter(Region == "WLD") %>%
  filter(Variable %in% c("FOOD", "FEED", "PROD", "OTHR", "EXPO"),
         Item %in% c("RUM", "NRM", "DRY", "WHT", "OSD", "SGC", "VFN", "CGR"),
         Year %in% c(2020, 2050)) %>%
  mutate(Value = Value / 1000, Year = as.character(Year)) %>%
  ggplot() + facet_grid(Variable~Item, scales = "free_x") +
  geom_point(aes(x = Scenario, y = Value, fill = Year), shape = 21, color ="black") +
  coord_flip() +
  labs(y = "Mt", x = "Scenario",
       title = "MidCentury changes in Ag supply-demand by key sector across AgMIP Scenarios",
       subtitle = "CGR (Corn & other grains), DRY (Dairy), NRM (NonRum meat), OSD (oilseeds), RUM (Rum mean) \nSGC (sugar crops), VFN (Veg., Fruits, Nutts), WHT (Wheat)" ) +
  theme_bw() + theme0 + theme1 -> p

p %>% Write_png(.name = "Quantity_SUA", .DIR_MODULE = DIR_MODULE, h = 14, w = 14, r = 300)


# Value SUA_FOOD_FEED_TRADE_OTHR_PROD ----

AgElement_AgMIP3 %>%
  spread(element, value) %>%
  transmute(Scenario = scenario, Region = region, Year = year, Item = AgMIP,
            PROD = Production, Varialbe = "XPRP",
            Value = Revenue / Production * gdp_deflator(2015, 1975)*1000,
            Unit = "2015 USD/t" ) %>%
  filter(Item != "FSH_OAP") ->
  XPRP

XPRP %>% filter(Value <0)

AgElement_AgMIP3 %>%
  spread(element, value) %>%
  transmute(Scenario = scenario, Region = region, Year = year, Item = AgMIP,
            Variable = "XPRP",
            Value = Revenue / Production * gdp_deflator(2015, 1975)*1000,
            Unit = "2015 USD/t", PROD = Production ) %>%
  filter(Item != "FSH_OAP") ->
  XPRP

XPRP %>%  rename(weight = PROD) %>%
  AgMIP_AggReg(WMean = T) %>%
  filter(!is.na(Value))-> XPRP_All

XPRP_All %>% mutate(Variable = "XPRX") %>%
  bind_rows(XPRP_All) ->
  XPRP_XPRX_All

# ***XPRP_XPRX_All----
Add_To_AgMIP_Report(XPRP_XPRX_All)

XPRP_XPRX_All %>%
  filter(Region == "WLD", Variable == "XPRP") %>%
  group_by(Scenario) %>%
  mutate(Value = Value / Value[Year == 2020]) %>%
  spread(Year, Value)

XPRP %>% filter(!is.na(Value)) %>%
  group_by_at(vars(-Region, -Item, -Value, -PROD)) %>%
  summarize(Value = weighted.mean(Value, w = PROD), .groups = "drop") %>%
  ggplot() + #facet_grid(Item~Region, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = Year, y = Value, color = Scenario)) +
  labs(y = "2015$ per tonne", x = "Year",
       title = "Producer prices") +
  theme_bw() + theme0 + theme1 -> p
p %>% Write_png(.name = "Prices_Producer_WLD", .DIR_MODULE = DIR_MODULE, h = 6, w = 8, r = 300)

XPRP %>% #filter(Item == "NRM") %>%
  ggplot() + facet_grid(Item~Region, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = Year, y = Value, color = Scenario)) +
  labs(y = "2015$ per tonne", x = "Year",
       title = "Producer prices") +
  theme_bw() + theme0 + theme1 -> p

p %>% Write_png(.name = "Prices_Producer", .DIR_MODULE = DIR_MODULE, h = 16, w = 18, r = 300)



## Done SUA ----
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

# CALO CALI ----


readr::read_csv("data/inst/GCAM_region_names.csv", comment = "#") ->
  GCAM_region_names
readRDS("data/inst/GCAM_FoodWaste_Share_Pathway.rds") %>%
  left_join_error_no_match(GCAM_region_names) ->
  GCAM_FoodWaste_Share_Pathway

"Fooddemand" %>% PluckBind() %>%
  Agg_reg(region, technology) %>%
  distinct(technology) %>% pull


ListJuly2024 %>% purrr::pluck("FooddemandExo") %>%
  select(-ss) %>% filter(year %in% 2015:2050) %>%
  filter(scenario %in% c("BAU_DIET", "EL2", "ELM", "ELM_NoCC", "ELM_PROD", "ELM_WAST")) %>%
  Agg_reg(region, technology) %>%
  bind_rows(
    ListJuly2024 %>% purrr::pluck("Fooddemand") %>%
      select(-ss) %>% filter(year %in% 2015:2050) %>%
      filter(!scenario %in% c("BAU_DIET", "EL2", "ELM", "ELM_NoCC", "ELM_PROD", "ELM_WAST")) %>%
      Agg_reg(region, technology)
  ) ->
  FoodPcal

FoodPcal %>%
  left_join_error_no_match(
    GCAM_FoodWaste_Share_Pathway %>% rename(technology = GCAM_commodity),
    by = c("region", "technology", "year")
  ) %>%
  mutate(avail = if_else(!scenario %in% c("EL2", "ELM","ELM_WAST", "BAU_WAST"),
                          value / (1-WasteShare),
                          value / (1-HalfWaste2050)) ) %>%
  rename(AgCOMM = technology) %>%
  left_join_error_no_match(
    MapAgCOMM %>% distinct(AgCOMM, AgMIP) %>%
      filter(!is.na(AgMIP)), by = "AgCOMM"
  ) %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = AgMIP13),
                           by = "region0") %>%
  group_by(Scenario = scenario, Region = region, Item = AgMIP, Year = year) %>%
  summarize(intake = sum(value), avail = sum(avail), .groups = "drop") ->
  FoodPcal_1

FoodPcal_1 %>%
  #group_by(Scenario, Region, Year) %>%
  #summarize(intake = sum(intake), avail = sum(avail), .groups = "drop") %>%
  left_join_error_no_match(POPT %>%
                             select(Scenario, Region, Year, POP = Value),
                           by = c("Scenario", "Region", "Year")) %>%
  mutate(intake = intake /365/POP * 1000000,
         avail = avail /365/POP * 1000000) ->
  FoodPcal_2

# Need to have a method to allocate NEC and separate FSH_OAP

readRDS("data/inst/GCAM_OtherMeat_FishShare.RDS") -> GCAM_OtherMeat_FishShare

GCAM_OtherMeat_FishShare %>%
  left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = AgMIP13),
                           by = "region0") %>%
  group_by(Region = region) %>%
  summarize(FishShare = sum(Fish)/sum(Fish + OtherMeat), .groups = "drop") ->
  GCAM_OtherMeat_FishShare_AgMIP

# Separating fish and OAP
FoodPcal_2 %>%
  filter(Item == "FSH_OAP") %>%
  left_join(GCAM_OtherMeat_FishShare_AgMIP, by = "Region") %>%
  mutate(intake = FishShare *intake, avail = FishShare * avail) %>%
  mutate(Item = "FSH") %>%
  bind_rows(
    FoodPcal_2 %>%
      filter(Item == "FSH_OAP") %>%
      left_join(GCAM_OtherMeat_FishShare_AgMIP, by = "Region") %>%
      mutate(intake = (1-FishShare) * intake, avail = (1-FishShare) * avail) %>%
      mutate(Item = "OAP")
  ) %>% select(-FishShare) %>%
  bind_rows(FoodPcal_2 %>%
              filter(Item != "FSH_OAP") ) ->
  FoodPcal_3


readRDS("data/inst/GCAM_FoodCalAvail_NECShare.RDS") -> GCAM_FoodCalAvail_NECShare

GCAM_FoodCalAvail_NECShare %>%
  left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = AgMIP13),
                           by = "region0") %>%
  group_by(Region = region) %>%
  summarize(NECScaler = 1+ sum(MKcal)/sum(total - MKcal), .groups = "drop") ->
  GCAM_FoodCalAvail_NECScaler_AgMIP

# Add CRP and LSP
FoodPcal_3 %>%
  filter(Item %in% c("OAP", "RUM", "NRM", "DRY")) %>%
  mutate(Item = "LSP") %>%
  group_by_at(vars(-intake, -avail)) %>%
  summarize(intake = sum(intake), avail = sum(avail), .groups = "drop") %>%
  bind_rows(
    FoodPcal_3 %>%
      filter(!Item %in% c("OAP", "RUM", "NRM", "DRY", "FSH")) %>%
      mutate(Item = "CRP") %>%
      group_by_at(vars(-intake, -avail)) %>%
      summarize(intake = sum(intake), avail = sum(avail), .groups = "drop")
  ) ->
  FoodPcal_3_CRP_LSP

# CRP + LSP + NEC = AGR
FoodPcal_3 %>%
  mutate(Item = "AGR") %>%
  group_by_at(vars(-intake, -avail)) %>%
  summarize(intake = sum(intake), avail = sum(avail), .groups = "drop") %>%
  left_join_error_no_match(
    GCAM_FoodCalAvail_NECScaler_AgMIP, by = "Region"
  ) %>%
  mutate(intake = intake * (1+ NECScaler),
         avail = avail * (1+ NECScaler)) %>%
  select(-NECScaler) ->
  FoodPcal_3_AGR

FoodPcal_3 %>%
  bind_rows(FoodPcal_3_CRP_LSP) %>%
  bind_rows(FoodPcal_3_AGR) %>%
  rename(CALO = avail, CALI = intake) %>%
  gather(Variable, Value, CALO, CALI)->
  FoodPcal_4

# Add other regions

FoodPcal_4 %>% rename(weight = POP) %>%
  AgMIP_AggReg(WMean = T) %>%
  mutate(Unit = "kcal/cap/d") ->
  FoodPcal_All
# *** FoodPcal_All ----
Add_To_AgMIP_Report(FoodPcal_All)

FoodPcal_All %>% filter(Year == 2050) %>%
  filter(!Item %in% c("AGR", "CRP", "LSP")) %>%
  filter(Region %in% c("WLD", "SSA", "USA")) %>%
  ggplot() + facet_grid(Region ~Variable) +
  geom_bar(aes(x = Scenario, y = Value, fill = Item), color = 1,
           stat = "identity") +
  coord_flip() +
  labs(y = "kcal/ca/d", x = "Scenario",
       title = "Mid-Century food calorie intake (CALI) and supply (CALO), NEC not included") +
  theme_bw() + theme0 -> p

p %>% Write_png(.name = "Food_Calorie2050", .DIR_MODULE = DIR_MODULE, h = 16, w = 16, r = 300)



# Wage AWAG Ag Labor ----

ListJuly2024 %>% purrr::pluck("LaborWage") %>%
  select(-ss) %>% filter(year %in% 2015:2050) %>%
  mutate(scenario = factor(scenario,
                           levels =  c(AgMIP_Reporting %>% distinct(Scenario) %>%
                                         filter(!is.na(Scenario)) %>% pull))) %>%
  mutate(region = gsub("Labor_Ag", "", market)) %>%
  left_join(
    ListJuly2024 %>% purrr::pluck("LaborInputSector") %>%
      select(-ss) %>% filter(year %in% 2015:2050) %>%
      Agg_reg(region) %>% rename(weight = value), by = c("scenario", "region", "year")
  ) %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = AgMIP13),
                           by = "region0") %>%
  group_by(scenario, year, region) %>%
  summarize(Value = weighted.mean(value, w = weight), weight = sum(weight),
            .groups = "drop") %>%
  transmute(
    Scenario = scenario, Region = region, Year = year,
    Item = "AGR", Variable = "AWAG", Value = Value * gdp_deflator(2015, 1975) * 1000,
    Unit = "2015 USD/person", weight
  ) -> AWAG_1


AWAG_1 %>%
  AgMIP_AggReg(WMean = T) ->
  AWAG_All

# ***AWAG_All ----
Add_To_AgMIP_Report(AWAG_All)


AWAG_All %>%
  #filter(Region %in% c("WLD", "SSA", "USA")) %>%
  group_by_at(vars(-Value, -Year)) %>%
  mutate(Index = Value / first(Value)) %>%
  ungroup() %>%
  ggplot() + facet_wrap(~Region) +
  geom_line(aes(x = Year, y = Index, color = Scenario)) +
  labs(y = "2015$ per person", x = "Scenario",
       title = "Wage rate") +
  theme_bw() + theme0 -> p;p

p %>% Write_png(.name = "AgWage", .DIR_MODULE = DIR_MODULE, h = 16, w = 16, r = 300)


"LaborInputSector" %>% PluckBind() %>%
  Agg_reg(region) %>%
  transmute(
    Scenario = scenario, Region = region, Year = year,
    Item = "AGR", Variable = "AEMP_abs", Value = value,
    Unit = "Million"
  ) ->
  AEMP_abs_1

AEMP_abs_1 %>%
  AgMIP_AggReg() ->
  AEMP_abs_All

# ***AEMP_abs_All ----
Add_To_AgMIP_Report(AEMP_abs_All)

# AEMP_prcn ----

readRDS("data/inst/LaborForce.RDS") -> LaborForce

LaborForce %>% ungroup() %>%
  filter(scenario == "ssp2") %>% select(-scenario) %>%
  left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = AgMIP13),
                           by = "region0") %>%
  group_by(Region = region, Year = year) %>%
  summarize(LF = sum(value)) ->
  AgMIP_LF

AEMP_abs_All %>%
  left_join(AgMIP_LF, by = c("Region", "Year")) %>%
  filter(!is.na(LF)) %>%
  mutate(Variable = "AEMP_prcn", Unit = "% of labor force",
         Value = Value / LF * 100 ) ->
  AEMP_prcn_1

AEMP_prcn_1 %>% filter(Scenario == "BAU", Year == 2015)

AEMP_prcn_1 %>%
  rename(weight = LF) %>%
  AgMIP_AggReg(WMean = T) ->
  AEMP_prcn_All
# ***AEMP_prcn_All ----
Add_To_AgMIP_Report(AEMP_prcn_All)

AEMP_prcn_All %>%
  filter(Region %in% c("WLD", "SSA", "USA")) %>%
  ggplot() + facet_wrap(~Region, scales = "free_y") +
  geom_line(aes(x = Year, y = Value, color = Scenario)) +
  labs(y = "%", x = "Year",
       title = "Ag employment share of labor force") +
  theme_bw() + theme0 -> p;p

p %>% Write_png(.name = "AgLaborShare", .DIR_MODULE = DIR_MODULE, h = 10, w = 14, r = 300)





## Compile data ----

MapAgCOMM %>% pull(regsector)

# get data for AgElement ----

PluckBind <- function(.query){
  ListJuly2024 %>% purrr::pluck(.query) %>%
    select(-ss) %>% filter(year %in% 2015:2050) %>%
    mutate(scenario = factor(scenario,
                             levels =  c(AgMIP_Reporting_Sets %>% distinct(Scenario) %>%
                                           filter(!is.na(Scenario)) %>% pull))) %>%
    mutate(branch = "AgMIP")
  #%>%
    # rename(region0 = region) %>%
    # left_join_error_no_match(Regmapping %>% select(region0 = region, region = AgMIP13),
    #                          by = "region0") %>%
    # group_by_at(vars(-region0, -value)) %>%
    # summarize(value = sum(value), .groups = "drop") %>%
    # mutate(branch = "AgMIP")
}


## Start with demand ----
"AgBal" %>% PluckBind() %>%
  filter(Units == "Mt") %>%
  mutate(input = gsub("regional |_", "", input) ) %>%
  mutate(sector = if_else(grepl("NonFoodDemand", sector), "Other use", sector),
         sector = if_else(grepl("ethanol|biomassOil", sector), "Bioenergy", sector),
         sector = if_else(grepl("FoodDemand", sector), "Food", sector),
         sector = if_else(grepl("FeedCrops|Fodder", sector), "Feed", sector)) %>%
  Agg_reg(branch, region, element = sector, sector = input) %>%
  ## Bind storage ----
  bind_rows(
  "AgStorageClose" %>% PluckBind() %>%
    transmute(branch, scenario, year,region, sector, value, element = "ClosingStockBeforeLoss") %>%
    bind_rows("AgStorageOpen" %>% PluckBind()  %>% transmute(branch, scenario, year, region, sector, value, element = "Opening stocks")) %>%
    bind_rows("AgStorageCloseLoss" %>% PluckBind()  %>% transmute(branch, scenario, year,region, sector, value, element = "Closing stocks")) %>%
    mutate(sector = gsub("regional |_", "", sector)) %>%
    spread(element, value) %>%
    mutate(`Stock loss` = ClosingStockBeforeLoss -`Closing stocks`) %>%
    select(-ClosingStockBeforeLoss) %>%
    gather(element, value, -branch:-sector) ) %>%
  ## Bind Trade ----
bind_rows(
  "RegAgsource" %>% PluckBind() %>%
    mutate(sector = gsub("total |_", "", sector)) %>%
    filter(grepl("imported", subsector)) %>%
    Agg_reg(branch, region, sector, element = "Import") %>%
    bind_rows(
      "TradedAgsource" %>% PluckBind() %>%
        filter(Units == "Mt", !grepl("iron", sector)) %>%
        mutate(sector = gsub("traded |_", "", sector)) %>%
        group_by(sector) %>%
        mutate(subsector = gsub(unique(sector),"", subsector),
               subsector = gsub(" traded ","", subsector),
               subsector = gsub("nuts_seeds|root_tuber", "", subsector) ) %>%
        ungroup() %>%
        transmute(branch, scenario, region = subsector, sector, year, value, element = "Export")
    )
) %>%
  # Bind Production ----
bind_rows(
  "Agprod" %>% PluckBind() %>%
    filter(Units == "Mt") %>%
    Agg_reg(branch, region, sector = tolower(sector), element = "Production") %>%
    bind_rows(
      "MeatProd" %>% PluckBind() %>% filter(Units == "Mt") %>%
        Agg_reg(branch, region, sector = tolower(sector), element = "Production")
    )
) -> AgElement_SUA


AgElement_SUA %>% filter(element == "Production") %>%
  spread(element, value) %>%
  inner_join(
    "Aggland" %>% PluckBind() %>%
      mutate(LandLeaf = gsub("C4$|Tree$", "", LandLeaf) ) %>%
      group_by(scenario, region, sector = LandLeaf, year) %>%
      summarise(Area = sum(value)/10, .groups = "drop") %>% # to Mha
      mutate(sector = tolower(sector)),
    by = c("scenario", "region", "sector", "year")
  ) %>%
  mutate(Yield = Production / Area) %>% select(-Production) %>%
  gather(element, value, Area, Yield) %>%
  bind_rows(
    AgElement_SUA %>% filter(element == "Production") %>%
      spread(element, value) %>%
      left_join_error_no_match(
        "Agprices" %>% PluckBind() %>%
          bind_rows("Meatprices" %>% PluckBind()) %>%
          mutate(sector = tolower(sector)) %>%
          rename(Price = value) %>% select(-Units) ) %>%
      mutate(Revenue = Production * Price) %>%
      select(-Production) %>%
      gather(element, value, Revenue, Price)
  ) ->
  AgElement_AreaYieldPrice


AgElement_SUA %>%
  bind_rows(AgElement_AreaYieldPrice) -> AgElement




# Func to add data to AgMIP reporting ----

rm(AgMIP_Report)
Add_To_AgMIP_Report <- function(.DF,
                                outputlistnm = "AgMIP_Report"){

  AgMIP_Report_Names <-  c("Scenario", "Region", "Item", "Year", "Variable", "Unit", "Value")
  assertthat::assert_that(names(.DF) %in% (AgMIP_Report_Names %>% all_of()) %>% all())
  assertthat::assert_that(!anyNA(.DF %>% select(AgMIP_Report_Names)))

  if (!exists(outputlistnm, envir = .GlobalEnv)) {
    # Create an empty list
    list() -> outputlisttemp

    # add to list
    outputlisttemp[[deparse(substitute(.DF))]] <- .DF
    # to global env
    assign(outputlistnm, outputlisttemp, envir = .GlobalEnv)

  }else{
    outputlisttemp <- get(outputlistnm)

    # add to list
    outputlisttemp[[deparse(substitute(.DF))]] <- .DF
    # to global env
    assign(outputlistnm, outputlisttemp, envir = .GlobalEnv)

  }
}
Add_To_AgMIP_Report(.DF = GDPT)

AgMIP_AggReg <- function(.DF, WMean = F){

  .DF %>%
    mutate(Region = "WLD") %>%
    bind_rows(
      .DF %>%
        filter(Region %in% c("CAN", "USA")) %>% mutate(Region = "NAM")
    ) %>%
    bind_rows(
      .DF %>%
        filter(Region %in% c("BRA", "OSA")) %>% mutate(Region = "OAM")
    ) %>%
    bind_rows(
      .DF %>%
        filter(Region %in% c("MEN", "SSA")) %>% mutate(Region = "AME")
    ) %>%
    bind_rows(
      .DF %>%
        filter(Region %in% c("CHN", "IND", "SEA", "OAS")) %>% mutate(Region = "SAS")
    ) -> .DF1

  if(WMean == TRUE){


    .DF1 %>%
      group_by_at(vars(-Value, -weight)) %>%
      summarize(Value = weighted.mean(Value, w = weight), .groups = "drop") %>%
      bind_rows(
        .DF %>% select(-weight)
      )


  }else{

    .DF1 %>%
      group_by_at(vars(-Value)) %>%
      summarize(Value = sum(Value), .groups = "drop") %>%
      bind_rows(
        .DF
      )
  }

}


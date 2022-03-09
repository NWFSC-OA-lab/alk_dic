# functions for com_alk_ph_app ----

## libraries ----
library(tidyverse)
library(seacarb)

## filter alk function ----
filter_table <- function(d, filter_alk_lab, filter_run_by, filter_experiment,
                       filter_unit, filter_unit_id, filter_water_source, filter_water_type,
                       filter_sample_set, filter_date_collected, filter_treatment_name, filter_quality_flag){
  
  
  #convert missing values from checkboxes (i.e. "", to NA)
  filter_run_by[filter_run_by == ""] <- NA
  filter_experiment[filter_experiment == ""] <- NA
  filter_unit[filter_unit == ""] <- NA
  filter_unit_id[filter_unit_id == ""] <- NA
  filter_water_source[filter_water_source == ""] <- NA
  filter_water_type[filter_water_type == ""] <- NA
  filter_sample_set[filter_sample_set == ""] <- NA
  filter_date_collected[filter_date_collected == ""] <- NA
  filter_treatment_name[filter_treatment_name == ""] <- NA
  
  if(!is.null(filter_alk_lab)){
    filter_alk_lab[filter_alk_lab == ""] <- NA
    d <- d %>%
      filter(alk_lab %in% filter_alk_lab)
  }
  if(!is.null(filter_run_by)){
    d <- d %>%
      filter(run_by %in% filter_run_by)
  }
  if(!is.null(filter_experiment)){
    d <- d %>% 
      filter(experiment %in% filter_experiment)
  }
  if(!is.null(filter_unit)){
    d <- d %>% 
      filter(unit %in% filter_unit)
  }
  if(!is.null(filter_unit_id)){
    d <- d %>% 
      filter(unit_id %in% filter_unit_id)
  }
  if(!is.null(filter_water_source)){
    d <- d %>%
      filter(water_source %in% filter_water_source)
  }
  if(!is.null(filter_water_type)){
    d <- d %>%
      filter(water_type %in% filter_water_type)
  }
  if(!is.null(filter_sample_set)){
    d <- d %>% 
      filter(sample_set %in% filter_sample_set)
  }
  if(!is.null(filter_date_collected)){
    d <- d %>%
      filter(as.character(date_collected) %in% filter_date_collected)
  }
  if(!is.null(filter_treatment_name)){
    d <- d %>%
      filter(treatment_name %in% filter_treatment_name)
  }
  if(!is.null(filter_quality_flag)){
    filter_quality_flag[filter_quality_flag == ""] <- NA
    d <- d %>%
      filter(quality_flag %in% filter_quality_flag)
  }
  
  return(d)
}

make_match <- function(d_alk, d_ph){
  d_match <- data.frame(  alk_n = nrow(d_alk), 
                          alk = mean(d_alk$alkalinity), 
                          alk_salinity = mean(d_alk$salinity),
                          ph_n = nrow(d_ph),
                          ph = mean(d_ph$pHinsitu),
                          temperature = mean(d_ph$insituTemp)) 
   d_carb <- carb(flag = 8, d_match$ph, d_match$alk/1000000, T = d_match$temperature, S = d_match$alk_salinity)
   
   d_alk_1 <- d_alk %>%
     slice_head() %>%
     rename_with(~paste("alk", .x, sep = "_"))
   
   d_ph_1 <- d_ph %>%
     slice_head() %>%
     rename_with(~paste("ph", .x, sep = "_"))
   
   d_match <- cbind(d_match, d_carb, d_alk_1, d_ph_1)
     
     
  return(d_match)
}











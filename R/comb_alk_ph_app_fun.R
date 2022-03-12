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

## ph insitu function ----
ph_insitu_fun <- function(ph_spec, temperature_spec, temperature_insitu, 
                          alkaliniy, salinity){
  ph_insitu <- NA
  dic <- carb(flag = 8, ph_spec, alkaliniy / 1000000, 
              T = temperature_spec, S = salinity)$DIC
  ph_insitu <- carb(flag = 15, alkaliniy / 1000000, dic, 
                    T = temperature_insitu, S = salinity)$pH
  return(ph_insitu)
}

## make_match function ----
make_match <- function(d_alk, d_ph){
  d_match <- data.frame(  alk_n = nrow(d_alk), 
                          alk_mean = mean(d_alk$alkalinity),
                          alk_salinity_mean = mean(d_alk$salinity),
                          ph_n = nrow(d_ph),
                          ph_spec_est_of_alk_mean = mean(d_ph$pHinsitu),
                          spec_est_of_alk_mean = mean(d_ph$alk),
                          ph_spec_temp_mean = mean(d_ph$pHat25),
                          ph_salinity_mean = mean(d_ph$salinity),
                          ph_insitu_temp_mean = mean(d_ph$insituTemp)) %>%
    mutate(ph_this_est_alk_mean = ph_insitu_fun(ph_spec_temp_mean, ph_spec_temp_mean,
                                                ph_insitu_temp_mean, alk_mean, 
                                                ph_salinity_mean))
   
  d_carb_spec_est <- carb(flag = 8, d_match$ph_spec_est_of_alk_mean, 
                           d_match$spec_est_of_alk_mean/1000000, 
                           T = d_match$ph_insitu_temp_mean, S = d_match$ph_salinity_mean) %>%
     rename_with(~paste("spec_est", .x, sep = "_"))
   
  d_carb_alk_est <- carb(flag = 8, d_match$ph_this_est_alk_mean, d_match$alk_mean/1000000, 
                           T = d_match$ph_insitu_temp_mean, S = d_match$ph_salinity_mean) %>%
     rename_with(~paste("this_est", .x, sep = "_"))
   
   d_alk_1 <- d_alk %>%
     slice_head() %>%
     rename_with(~paste("alk", .x, sep = "_"))
   
   d_ph_1 <- d_ph %>%
     slice_head() %>%
     rename_with(~paste("ph", .x, sep = "_"))
   
   d_match <- cbind(d_match, d_carb_spec_est, d_carb_alk_est, d_alk_1, d_ph_1)
     
     
  return(d_match)
}











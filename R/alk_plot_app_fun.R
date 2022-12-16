# functions for alk_plot_app ----

## libraries ----
library(tidyverse)

## filter alk function ----
filter_alk <- function(d_alk, filter_alk_lab, filter_run_by, filter_experiment,
                       filter_unit, filter_unit_id, filter_water_source, filter_water_type,
                       filter_sample_set, filter_date_collected, filter_date_run, 
                       filter_treatment_name, filter_quality_flag){
  
  #convert missing values from checkboxes (i.e. "", to NA)
  filter_alk_lab[filter_alk_lab == ""] <- NA
  filter_run_by[filter_run_by == ""] <- NA
  filter_experiment[filter_experiment == ""] <- NA
  filter_unit[filter_unit == ""] <- NA
  filter_unit_id[filter_unit_id == ""] <- NA
  filter_water_source[filter_water_source == ""] <- NA
  filter_water_type[filter_water_type == ""] <- NA
  filter_sample_set[filter_sample_set == ""] <- NA
  filter_date_collected[filter_date_collected == ""] <- NA
  filter_date_run[filter_date_run == ""] <- NA
  filter_treatment_name[filter_treatment_name == ""] <- NA
  filter_quality_flag[filter_quality_flag == ""] <- NA
  
  
  if(!is.null(filter_alk_lab)){
    d_alk <- d_alk %>% 
           filter(alk_lab %in% filter_alk_lab)
  }
  if(!is.null(filter_run_by)){
    d_alk <- d_alk %>% 
      filter(run_by %in% filter_run_by)
  }
  if(!is.null(filter_experiment)){
    d_alk <- d_alk %>% 
      filter(experiment %in% filter_experiment)
  }
  if(!is.null(filter_unit)){
    d_alk <- d_alk %>% 
      filter(unit %in% filter_unit)
  }
  if(!is.null(filter_unit_id)){
    d_alk <- d_alk %>% 
      filter(unit_id %in% filter_unit_id)
  }
  if(!is.null(filter_water_source)){
    d_alk <- d_alk %>%
      filter(water_source %in% filter_water_source)
  }
  if(!is.null(filter_water_type)){
    d_alk <- d_alk %>%
      filter(water_type %in% filter_water_type)
  }
  if(!is.null(filter_sample_set)){
    d_alk <- d_alk %>% 
      filter(sample_set %in% filter_sample_set)
  }
  if(!is.null(filter_date_collected)){
    d_alk <- d_alk %>%
      filter(as.character(date_collected) %in% filter_date_collected)
  }
  if(!is.null(filter_date_run)){
    d_alk <- d_alk %>%
      filter(as.character(date_run) %in% filter_date_run)
  }
  if(!is.null(filter_treatment_name)){
    d_alk <- d_alk %>%
      filter(treatment_name %in% filter_treatment_name)
  }
  if(!is.null(filter_quality_flag)){
    d_alk <- d_alk %>%
      filter(quality_flag %in% filter_quality_flag)
  }
  
  return(d_alk)
}


## plot function ----
alk_plot <- function(d_alk_filtered, plot_type, y_var, x_axis_vars, 
                         color_by_vars, facet_by_vars,
                     point_size, set_y_range, y_range, font_size, show_alk_sal_est,
                     rotate_x_axis, show_ref_value, free_y_facet){
  
  # A color-blind friendly palette :
  cbPalette <- c("#E69F00","#56B4E9","#009E73",
                 "#F0E442","#0072B2","#D55E00","#CC79A7")
  
  base_color <- cbPalette[5]
  mean_color <- "red"
  
  # add x-axis variable
  # add x-axis variable
  # if there is only one variable, keep it in original class (e.g. date)
  # if more than one variable, it becomes a character 
  if(length(x_axis_vars) == 1){
    d_alk_plot  <- as.data.frame(d_alk_filtered)
    d_alk_plot$x_axis <- d_alk_plot[,x_axis_vars]
  }else{
    d_alk_plot  <- d_alk_filtered %>%
      unite(x_axis, x_axis_vars, remove = FALSE)
  }
  
  d_alk_plot  <- d_alk_plot %>%
    mutate(y_variable = case_when(y_var == "alkalinity" ~ alkalinity,
                                  y_var == "salinity" ~ salinity,
                                  TRUE ~ NA_real_))
  
  y_facet <- if_else(free_y_facet, "free_y", "fixed")
  
  # return null default
  p <- NULL
  
  #TODO fix plotting alk_sal_est 
  ### box plots
  if(plot_type == "box" & is.null(color_by_vars) & is.null(facet_by_vars)){
    p <- d_alk_plot %>%
      ggplot(aes(x_axis, y_variable)) +
      geom_boxplot(colour = base_color) +
      geom_jitter(width = 0.1, height = 0, alpha = 0.5, size = point_size, colour = base_color) +
      stat_summary(fun="mean", geom="point", shape=8, size=14, color=mean_color, fill="black")
    if(show_alk_sal_est){
      p <- p +  stat_summary(aes(x_axis, alk_sal_est),fun=base::mean, geom="point", na.rm = TRUE,
                             shape=18, size=14, color="purple", fill="purple") 
    }
  }
  
  if(plot_type == "box" & !is.null(color_by_vars) & is.null(facet_by_vars)){
    p <-  d_alk_plot %>%
      unite(color_by, color_by_vars, remove = FALSE) %>%
      ggplot(aes(x_axis, y_variable, color = color_by)) +
        geom_boxplot() +
        geom_point(alpha = 0.5, size = point_size,
                   position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0)) +
        stat_summary(fun="mean", geom="point", shape=8, size=14, 
                     position = position_dodge2(width = 0.9, preserve = "single")) +
        scale_colour_manual(name = paste(color_by_vars, collapse = "_"),
                            values = cbPalette)
    if(show_alk_sal_est){
      p <- p +  stat_summary(aes(x_axis, alk_sal_est, colour = color_by),fun="mean", 
                             na.rm = TRUE, geom="point", shape=18, size=14) 
    }
  }
  
  
  if(plot_type == "box" & is.null(color_by_vars) & !is.null(facet_by_vars)){
    p <-  d_alk_plot %>%
      unite(facet_by, facet_by_vars, remove = FALSE) %>%
      ggplot(aes(x_axis, y_variable)) +
      geom_boxplot(colour = base_color) +
      geom_jitter(width = 0.1, height = 0, alpha = 0.5, 
                  size = point_size, colour = base_color) +
      stat_summary(fun="mean", geom="point", shape=8, size=14, color=mean_color, fill="black") +
      facet_wrap(vars(facet_by), scales = y_facet)
    if(show_alk_sal_est){
      p <- p +  stat_summary(aes(x_axis, alk_sal_est),fun="mean", geom="point", na.rm = TRUE,
                             shape=18, size=14, color="purple", fill="purple") 
    }
  }  
  
  
  if(plot_type == "box" & !is.null(color_by_vars) & !is.null(facet_by_vars)){
    p <-  d_alk_plot %>%
      unite(color_by, color_by_vars, remove = FALSE) %>%
      unite(facet_by, facet_by_vars, remove = FALSE) %>%
      ggplot(aes(x_axis, y_variable, color = color_by)) +
        geom_boxplot() +
        geom_point(alpha = 0.5, size = point_size,
             position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0)) +
        stat_summary(fun="mean", geom="point", shape=8, size=14, 
                     position = position_dodge2(width = 0.9, preserve = "single")) +
        facet_wrap(vars(facet_by), scales = y_facet) + 
        scale_colour_manual(name = paste(color_by_vars, collapse = "_"),
                            values = cbPalette)
    if(show_alk_sal_est){
      p <- p +  stat_summary(aes(x_axis, alk_sal_est, color = color_by), fun="mean", 
                             na.rm = TRUE, geom="point", shape=18, size=14) 
    }
  }
  
  
  ### scatter plots
  if(plot_type == "scatter" & is.null(color_by_vars) & is.null(facet_by_vars)){
    p <- d_alk_plot %>%
      ggplot(aes(x_axis, y_variable)) +
      geom_point(size = point_size, colour = base_color) 
  }
  
  if(plot_type == "scatter" & !is.null(color_by_vars) & is.null(facet_by_vars)){
    p <-  d_alk_plot %>%
      unite(color_by, color_by_vars, remove = FALSE) %>%
      ggplot(aes(x_axis, y_variable, color = color_by)) +
        geom_point(size = point_size) +
        scale_colour_manual(name = paste(color_by_vars, collapse = "_"),
                          values = cbPalette)
  }
  
  if(plot_type == "scatter" & is.null(color_by_vars) & !is.null(facet_by_vars)){
    p <-  d_alk_plot %>%
      unite(facet_by, facet_by_vars, remove = FALSE) %>%
      ggplot(aes(x_axis, y_variable)) +
      geom_point(size = point_size, colour = base_color) +
      facet_wrap(vars(facet_by), scales = y_facet)
  }  
  
  if(plot_type == "scatter" & !is.null(color_by_vars) & !is.null(facet_by_vars)){
    p <-  d_alk_plot %>%
      unite(color_by, color_by_vars, remove = FALSE) %>%
      unite(facet_by, facet_by_vars, remove = FALSE) %>%
      ggplot(aes(x_axis, y_variable)) +
      geom_point(aes(color = color_by), size = point_size) +
      facet_wrap(vars(facet_by), scales = y_facet) + 
      scale_colour_manual(name = paste(color_by_vars, collapse = "_"),
                          values = cbPalette)
  }
  
  #set attributes common to all plots
  if(!set_y_range & y_var == "alkalinity"){
    y_range <- c(min(d_alk_plot$alkalinity), max(d_alk_plot$alkalinity))
  }
  if(!set_y_range & y_var == "salinity"){
    y_range <- c(min(d_alk_plot$salinity), max(d_alk_plot$salinity))
  }
  p <- p + 
    xlab(paste(x_axis_vars, collapse = "_")) +
    ylab(y_var) +
    ylim(y_range) +
    theme_bw(base_size = font_size)
  if(rotate_x_axis){
    p <- p +
      theme(axis.text.x = element_text(angle = 90))
  }
  if(show_ref_value){
    p <- p + 
      geom_point(aes(y = ref_value), size = 5, colour = "black", shape = 18)
  }
  
  return(p)
}












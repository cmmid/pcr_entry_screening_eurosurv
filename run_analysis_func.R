source("packages.R")

if (Sys.info()["nodename"] == "Sams-MacBook-Pro.local"){
  future::plan(multicore)
} else {
  future::plan(multiprocess)
  options(future.globals.maxSize = 2000*1024^2 )
}

source("utils.R")

run_analysis <- 
  function(n_arrival_sims  = 1000,
           countries       = c("EU", "USA"),
           trav_vol_manual = NULL,
           n_travellers    = 1e4,
           trav_vol_p      = 1,
           fixed           = TRUE,
           seed            = 145){
    
    set.seed(seed)
    #Parameters
    
    incubation_times <-
      make_incubation_times(
        n_travellers          = n_travellers,
        pathogen              = pathogen,
        syndromic_sensitivity = unique(input$syndromic_sensitivity))
    
    # Infected arrivals
    #browser()
    inf_arrivals <- make_inf_arrivals(
      countries       = countries,
      prev_est_region = prev_est_region,
      n_arrival_sims  = n_arrival_sims,
      asymp_fraction  = asymp_fraction,
      trav_vol_p      = trav_vol_p,
      flight_vols     = flight_vols,
      flight_times    = flight_times,
      trav_vol_manual = trav_vol_manual,
      incubation_times = incubation_times,
      fixed            = fixed)
    
    
    # Cross arrivals with scenarios
    arrival_scenarios <- make_arrival_scenarios(input, inf_arrivals, incubation_times)
    
    # Calculate when released
    arrival_released <- when_released(arrival_scenarios)
    
    # Calculate stage of infectiousness when released
    arrival_released_times <- stage_when_released(arrival_released)
    
    
    
    return(arrival_released_times)
    
  }

# baseline for comparison, least stringent
baseline_low <- data.frame(
  pre_board_screening   = NA,
  post_flight_screening = FALSE,
  first_test_delay      = 0,
  second_test_delay     = NA,
  stringency            = "low",
  label                 = "0 day quarantine, no test"
)

# baseline for comparison, most stringent but no test
baseline_high <- data.frame(
  pre_board_screening   = NA,
  post_flight_screening = FALSE,
  first_test_delay      = 14,
  second_test_delay     = NA,
  stringency            = "maximum",
  label                 = "14 day quarantine, no test"
)

run_rr_analysis <- 
  function(arrival_released_times,
           main_scenarios,
           baseline_scenario,
           text_size = 2.5,
           log_scale = FALSE,
           faceting = country ~ stringency){
   # browser()
    set.seed(145)

    #Parameters
    
    baseline <- inner_join(baseline_scenario, input)
    
    # arrival_released_times_summaries <- 
    #   make_arrival_released_quantiles(arrival_released_times, "country")
    
    stringencies <- distinct(arrival_released_times, stringency, scenario)
    
    facet_vars <- all.vars(faceting)
    
    if(!("pre_board_screening" %in% facet_vars)){
      arrival_released_times <- filter(arrival_released_times,
                                       is.na(pre_board_screening))
    }
    
    facet_vars_complete <- no_dots(grep(x = facet_vars, pattern = "stringency",
                                        invert = TRUE, value=T))
    
    arrival_released_times_summaries <- 
      # how come we don't have type in here?
      mutate(arrival_released_times, 
             time_in_iso = released_t - flight_arrival) %>% 
      group_by_at(   .vars = vars(stage_released, released_test, sim, scenario,
                                  country, !!!no_dots(facet_vars))) %>%
      tally 
    
    
    
    # up to here, needs looking into
    
    if (length(facet_vars_complete) > 0L){
      arrival_released_times_summaries %<>%
        ungroup %>%
        expand(., stage_released, released_test, sim, country, 
               !!sym(facet_vars_complete),
               nesting(scenario, stringency)) %>% 
        arrange(scenario) %>%
        left_join(arrival_released_times_summaries) %>% 
        replace_na(list(n= 0))      
    } else {
      arrival_released_times_summaries %<>%
        ungroup %>%
        expand(., stage_released, released_test, sim, country, 
               nesting(scenario, stringency)) %>% 
        arrange(scenario) %>%
        left_join(arrival_released_times_summaries) %>% 
        replace_na(list(n= 0))
    }
    

    baseline_summaries <- 
      inner_join(arrival_released_times_summaries,
                 baseline) %>% 
      filter(stage_released=="Infectious",
             grepl(x = released_test,
                   pattern ="Released after"),
             !grepl(x = released_test,
                    pattern = "\\+"))  %>%
      rename("baseline_n"             = n,
             "baseline_scenario"      = scenario,
             "baseline_released_test" = released_test,
             "baseline_stringency"    = stringency) %>%
      select(-contains("delay"), -pre_board_screening, -post_flight_screening,
             -pathogen, -syndromic_sensitivity)
    
    
    n_risk_ratios <- arrival_released_times_summaries %>% 
      filter(stage_released=="Infectious",
             grepl(x = released_test,
                   pattern ="Released after"),
             !grepl(x = released_test,
                    pattern = "\\+")) %>%
      inner_join(baseline_summaries) %>% 
      group_by_at(.vars = vars(sim, scenario, country,
                               #stringency,
                               one_of(facet_vars),
                               one_of(names(baseline)) )) %>%
      summarise_at(.vars = vars(n, baseline_n),
                   .funs = sum) %>%
      mutate(ratio=(n)/(baseline_n)) %>% 
      replace_na(list(ratio=1)) %>% 
      nest(data = c(ratio, n, baseline_n, sim)) %>%
      mutate(Q = map(.x = data, ~quantile(.x$ratio, probs = probs)),
             M = map_dbl(.x = data, ~mean(.x$ratio))) %>%
      unnest_wider(Q) %>%
      select(-data) %>%
      inner_join(stringencies) %>%
      inner_join(input)
    
    ylabA <- stringr::str_wrap(paste("Ratio of infectious persons released compared to",
                                     baseline_scenario$label), width = 40)
    
    rr_figs <-
      plot_data(input, n_risk_ratios, main_scenarios = NULL) %>%
      
      make_release_figure(
        x         = .,
        input     = input,
        text_size = text_size,
        xlab      = xlab,
        ylab      = ylabA, 
        log_scale = log_scale,
        faceting  = faceting,
        hline     = "dashed")  
    
    #browser()
    
    file <- paste(names(baseline_scenario),
                  baseline_scenario, sep = "-", 
                  collapse = " ")
    
   save_plot(
    device = c("png"),
    base="",
    prefix = paste0("rr_figs_baseline_",file),
    plot     = rr_figs,
    width    = 210, 
    height   = pmax(105,70*nrow(select(ungroup(n_risk_ratios),
                                            one_of(formula.tools::lhs.vars(faceting))) %>% 
                                       distinct)), 
    dpi      = 300)
    
    # write_csv(n_risk_ratios, paste0("results/",file,"_n_RR_results.csv"))
    # write.csv(pd_fig_data,paste0("results/baseline_",baseline_scenario,"_pd_RR_results.csv"))
    
    return(list(arrival_released_times = n_risk_ratios#,
                #n_fig_data             = n_fig_data,
                #pd_fig_data            = pd_fig_data
    ))
  }


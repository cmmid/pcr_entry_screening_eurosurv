  source('run_analysis_func.R')
  
  if (file.exists("results/results_10k.fst")){
    test_10k <- fst::read_fst("results/results_10k.fst")
  } else {
    
    test_10k <- run_analysis(
      n_arrival_sims  = 1000,
      countries       = c("EU","USA"),
      trav_vol_manual = 10000,
      fixed           = TRUE,
      n_travellers    = 1e4,
      trav_vol_p      = 1,
      seed            = 145)
    
    test_10k %>% 
      select(-where(is.list)) %>% 
      write.fst(.,"results/results_10k.fst")
    
  }
  
  test_10k_fig <-
    make_plots(arrival_released_times = test_10k, 
               input = input,
               text_size = 2.5,
               log_scale = FALSE,
               main_scenarios = main_scenarios,
               faceting = country ~ stringency,
               pre_board_screening = FALSE)
  
  save_plot(test_10k_fig,
            prefix = "10k",
            base = "main",
            width = 350, height = 150, units = "mm", device = NULL)
  
  test_10k_fig_type <-
    make_plots(arrival_released_times = test_10k, 
               input = input,
               text_size = 2.5,
               log_scale = FALSE,
               main_scenarios = main_scenarios,
               faceting = country + type ~ stringency,
               pre_board_screening = FALSE)
  
  save_plot(test_fig_type, 
            base = "type",
            prefix ="10k",
            height = 420, width = 210,
            device = c("pdf", "png"))
  
  
  if (file.exists("results/results_real.fst")){
    test_real <- fst::read_fst("results/results_real.fst")
  } else {
    
    test_real <- run_analysis(
      n_arrival_sims  = 1000,
      countries       = c("USA", "EU"),
      n_travellers    = 1e4,
      trav_vol_p      = 7/30,
      fixed           = FALSE,
      seed            = 145)
    
    
    test_real %>% 
      select(-where(is.list)) %>% 
      write.fst(.,"results/results_real.fst")
    
  }
  
  
  
  old_figure_3 <-
    make_plots(arrival_released_times = test_real, 
               input = input,
               text_size = 2.5,
               log_scale = FALSE,
               main_scenarios = main_scenarios,
               faceting = country ~ stringency,
               pre_board_screening = FALSE)
  
  save_plot(old_figure_3, 
            base = "fig_3_old",
            prefix ="real",
            height = 210/1.5, width = 210*1.5,
            device = c("png"))
  
  
  new_figure_3 <-
    make_plots(arrival_released_times = filter(test_real, 
                                               country == "USA"),
               pre_board_screening    = FALSE,
               input          = input,
               text_size      = 2.5,
               log_scale      = FALSE,
               main_scenarios = main_scenarios,
               faceting       = country ~ stringency)
  
  save_plot(new_figure_3, 
            base = "fig_3_new",
            prefix ="real",
            height = 210/1.5, width = 210*1.5,
            device = c("png"))
  
  
  new_figure_3_sum <-
    make_plots(arrival_released_times = filter(test_real, 
                                               country == "USA"),
               pre_board_screening    = FALSE,
               input          = input,
               text_size      = 2.5,
               log_scale      = FALSE,
               sum            = TRUE,
               main_scenarios = main_scenarios,
               faceting       = country ~ stringency)
  
  save_plot(new_figure_3_sum$figure, 
            base = "fig_3_new_sum",
            prefix ="real",
            height = 210/1.5, width = 210*1.5,
            device = c("png"))
  
  
  new_figure_3_sum_10k <-
    make_plots(arrival_released_times = filter(test_10k, 
                                               country == "USA"),
               pre_board_screening    = FALSE,
               input          = input,
               text_size      = 2.5,
               log_scale      = FALSE,
               sum            = TRUE,
               main_scenarios = main_scenarios,
               faceting       = country ~ stringency)
  
  save_plot(new_figure_3_sum_10k, 
            base = "fig_3_new_sum",
            prefix ="10k",
            height = 210/1.5, width = 210*1.5,
            device = c("png"))
  
  
  
  
  new_figure_3_symp <-
    make_plots(arrival_released_times = test_real %>%
                 filter(country == "USA"),
               sum = TRUE,
               input = input,
               text_size = 2.5,
               log_scale = FALSE,
               main_scenarios = main_scenarios,
               faceting = type + country ~ stringency,
               pre_board_screening = FALSE)
  
  save_plot(new_figure_3_symp$figure, 
            base   = "fig_3_new_symp",
            prefix = "real",
            height = 210/1.5,
            width  = 210*1.5,
            device = c("png"))
  
  
  
  
  new_figure_3_10k <-
    make_plots(arrival_released_times = test_10k,
               input = input,
               text_size = 2.5,
               log_scale = FALSE,
               main_scenarios = main_scenarios,
               faceting = country ~ stringency,
               days = FALSE,
               pre_board_screening = FALSE)
  
  
  save_plot(new_figure_3_10k$figure, 
            base   = "new_figure_3",
            prefix = "10k",
            height = 140, width = 210,
            device = c("png"))

  
  ## values for paper
  
  
  new_figure_3_sum$data$number %>%
    filter(time_in_iso == 0 | 
             (stringency == "moderate" & time_in_iso == 7) |
             (time_in_iso == 14)) %>%
    select(stringency, time_in_iso, country, contains('%'))
  
  new_figure_3_sum$data$days %>%
    filter(time_in_iso == 0 | (stringency == "moderate" & time_in_iso == 7)) %>%
    select(stringency, time_in_iso, country, contains('%'))
  
  rr_main$arrival_released_times %>%
    filter((stringency == "moderate" & first_test_delay %in% c(5,7) & is.na(second_test_delay) &
              #post_flight_screening == FALSE &
              country == "USA")) %>%
    select(stringency, country,first_test_delay, post_flight_screening, contains('%'))
  
  rr_main$arrival_released_times %>%
    filter((stringency == "maximum" & first_test_delay == 14 & is.na(second_test_delay) &
              #post_flight_screening == FALSE &
              country == "USA")) %>%
    select(stringency, country, contains('%'))
  
  rr_main$arrival_released_times %>%
    filter((stringency == "high" & first_test_delay + second_test_delay >= 8 &
              !is.na(second_test_delay) &
              #post_flight_screening == FALSE &
              country == "USA")) %>%
    select(stringency, country, first_test_delay, second_test_delay, contains('%'))
  
  
  # for abstract
  
  rr_main$arrival_released_times %>%
    replace_na(replace = list(second_test_delay = 0)) %>%
    filter((first_test_delay + second_test_delay >= 5 &
              !is.na(second_test_delay) &
              post_flight_screening == TRUE &
              country == "USA")) %>%
    select(stringency, country, first_test_delay, second_test_delay, contains('%'))
  
  
  ## appendix figures
  
  
  make_plots(arrival_released_times = test_real,
             pre_board_screening    = FALSE,
             input          = input,
             text_size      = 2.5,
             log_scale      = FALSE,
             sum            = TRUE,
             main_scenarios = main_scenarios,
             faceting       = country ~ stringency) %>%
    .['figure'] %>%
    save_plot(plot = ., 
              base   = "supp_main_both",
              prefix = "real",
              height = 210, width = 210*1.5,
              device = c("png"))
  
  make_plots(arrival_released_times = test_10k,
             pre_board_screening    = FALSE,
             input          = input,
             text_size      = 2.5,
             log_scale      = FALSE,
             sum            = TRUE,
             main_scenarios = main_scenarios,
             faceting       = country ~ stringency) %>%
    .['figure'] %>%
    save_plot(plot = ., 
              base   = "supp_main_both",
              prefix = "10k",
              height = 210, width = 210*1.5,
              device = c("png"))
  
  test_10k_supp_pbs <- 
    make_plots(arrival_released_times = test_10k,
               pre_board_screening    = TRUE,
               input          = input,
               text_size      = 2.5,
               log_scale      = FALSE,
               sum            = TRUE,
               main_scenarios = main_scenarios,
               faceting       = pre_board_screening ~ country + stringency) 
  
  save_plot(plot = test_10k_supp_pbs$figure, 
            base   = "supp_pbs_both",
            prefix = "10k", dpi = 200,
            height = 210*1.5, width = 210*3,
            device = c("png"))
  
  
  
  make_plots(arrival_released_times = test_10k,
             pre_board_screening    = FALSE,
             input          = input,
             text_size      = 2.5,
             log_scale      = FALSE,
             sum            = TRUE,
             main_scenarios = main_scenarios,
             faceting       = country + type ~ stringency) %>%
    .['figure'] %>%
    save_plot(plot = ., 
              base   = "supp_symp_both",
              prefix = "10k",
              height = 210, width = 210*1.5,
              device = c("png"))
  
  
  ## RR needs a fully infectious lot
  
  test_RR <- run_analysis(
    n_arrival_sims  = 100,
    countries       = "RR",
    trav_vol_manual = 1000,
    fixed           = TRUE,
    n_travellers    = 1e4,
    trav_vol_p      = 1,
    seed            = 145)
  
  rr_main_RR     <- run_rr_analysis(arrival_released_times = test_RR, 
                                    main_scenarios         = main_scenarios, 
                                    baseline_scenario      = baseline_low,
                                    faceting               = . ~ stringency)
  
  rr_high_RR     <- run_rr_analysis(arrival_released_times = test_RR, 
                                    main_scenarios         = main_scenarios, 
                                    baseline_scenario      = baseline_high,
                                    faceting               = . ~ stringency,
                                    log_scale              = TRUE)
  
  rr_main_RR_by_type     <- run_rr_analysis(arrival_released_times = test_RR, 
                                    main_scenarios         = main_scenarios, 
                                    baseline_scenario      = baseline_low,
                                    faceting               = type ~ stringency)
  
  
  rr_pbs_low <- run_rr_analysis(test_RR, main_scenarios, 
                                baseline_scenario = baseline_low,
                                faceting =  pre_board_screening ~ stringency)
  
  rr_pbs_high <- run_rr_analysis(test_RR, main_scenarios, 
                                 baseline_scenario = baseline_high,
                                 faceting =  pre_board_screening ~ stringency)

save_plot(plot = test_10k_supp_pbs$figure, 
          base   = "supp_pbs_both",
          prefix = "10k", dpi = 200,
          height = 210*1.5, width = 210*3,
          device = c("png"))



make_plots(arrival_released_times = test_10k,
           pre_board_screening    = FALSE,
           input          = input,
           text_size      = 2.5,
           log_scale      = FALSE,
           sum            = TRUE,
           main_scenarios = main_scenarios,
           faceting       = country + type ~ stringency) %>%
  .['figure'] %>%
  save_plot(plot = ., 
            base   = "supp_symp_both",
            prefix = "10k",
            height = 210, width = 210*1.5,
            device = c("png"))


## RR needs a fully infectious lot

test_RR <- run_analysis(
  n_arrival_sims  = 100,
  countries       = "RR",
  trav_vol_manual = 1000,
  fixed           = TRUE,
  n_travellers    = 1e4,
  trav_vol_p      = 1,
  seed            = 145)

rr_main_RR     <- run_rr_analysis(arrival_released_times = test_RR, 
                                  main_scenarios         = main_scenarios, 
                                  baseline_scenario      = baseline_low,
                                  faceting               = . ~ stringency)

rr_main_RR_type     <- run_rr_analysis(arrival_released_times = test_RR, 
                                       main_scenarios         = main_scenarios, 
                                       baseline_scenario      = baseline_low,
                                       faceting               = type ~ stringency)


rr_main_RR$arrival_released_times %>%
  ungroup %>%
  filter((stringency == "moderate" & first_test_delay %in% c(5,7) & is.na(second_test_delay))) %>%
  select(stringency,first_test_delay, post_flight_screening, contains('%')) %>%
  mutate_if(is.numeric, ~round(.,2))

rr_main_RR$arrival_released_times %>%
  ungroup %>%
  filter((stringency == "maximum" & first_test_delay == 14 & is.na(second_test_delay))) %>%
  select(stringency,first_test_delay, post_flight_screening, contains('%')) %>%
  mutate_if(is.numeric, ~round(.,2))

rr_main_RR$arrival_released_times %>%
  ungroup %>%
  filter((stringency == "high" & first_test_delay + second_test_delay >= 8 &
            !is.na(second_test_delay))) %>%
  select(stringency, first_test_delay, second_test_delay, contains('%')) %>%
  mutate_if(is.numeric, ~round(.,2))


# by symptom status

# reductions
rr_main_RR_type <- read_rds("results/rr_by_type.rds")

# test on arrival
rr_main_RR_type %>%
  ungroup %>%
  filter(stringency == "low",
         post_flight_screening == TRUE) %>%
  select(stringency, first_test_delay, type, post_flight_screening, contains('%')) %>%
  mutate_if(is.numeric, ~{1 -round(.,2)})

rr_main_RR_type %>%
  ungroup %>%
  filter(stringency == "moderate", first_test_delay == 9) %>%
  select(stringency, first_test_delay, type, post_flight_screening, contains('%')) %>%
  mutate_at(.vars = vars(contains("%")), .funs = ~{1 -round(., 2)}) %>%
  split(.$post_flight_screening)

# 14 day quarantine
rr_main_RR_type %>%
  ungroup %>%
  filter(stringency == "maximum", !post_flight_screening) %>%
  select(stringency, first_test_delay, type, post_flight_screening, contains('%')) %>%
  mutate_at(.vars = vars(contains("%")), .funs = ~{1 - round(., 2)})

rr_main_RR_type$arrival_released_times %>%
  ungroup %>%
  filter((stringency == "high" & first_test_delay + second_test_delay >= 7 &
            !is.na(second_test_delay))) %>%
  select(stringency, first_test_delay, second_test_delay, type, contains('%')) %>%
  mutate_at(.vars = vars(contains("%")), .funs = ~{1 -round(.,2)})



## test prior to arrival
# figure S4
rr_pbs <- run_rr_analysis(arrival_released_times = test_RR, 
                          main_scenarios         = main_scenarios, 
                          baseline_scenario      = baseline_low,
                          faceting               = pre_board_screening ~ stringency)

# effect of pre-flight screening
rr_pbs$arrival_released_times %>%
  filter(!is.na(pre_board_screening),
         stringency == "low",
         first_test_delay == 0,
         post_flight_screening == FALSE) %>%
  ungroup %>%
  select(pre_board_screening, stringency, contains("%")) %>%
  mutate_at(.vars = vars(contains("%")), .funs = ~{round(.,2)})


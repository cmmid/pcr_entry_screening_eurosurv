# need a separate function for plotting based on an arrival_released_times object

table_1       <- make_arrivals_table(inf_arrivals, c("country", "type"))
main_text_dat <- make_arrivals_table(inf_arrivals, c("country"))

figS2 <- make_plots(arrival_released_times,
                    input,
                    main_scenarios = main_scenarios,
                    faceting =  country + pre_board_screening ~ stringency )




#source("make_plots_pre.R",local = T)

list("png", "pdf") %>%
  map(~ggsave(filename = paste0("results/", 
                                plot_prefix, 
                                "_figS1_v2.",.x),
              plot = figS2,
              width = 297*2, height = 210*2, units="mm",
              dpi = 200))

#source("make_plots_alt.R",local = T)
fig3 <- make_plots(arrival_released_times %>%
                     filter(is.na(pre_board_screening)),
                   input,
                   main_scenarios = main_scenarios,
                   faceting =  country ~ stringency )

list("png", "pdf") %>%
  map(~ggsave(filename = paste0("results/", 
                                plot_prefix, 
                                "_fig3_v2.",.x),
              plot = fig3,
              width = 210, height = 280, units="mm",
              dpi = 320))


source("make_plots_type.R",local = T)

list("png", "pdf") %>%
  map(~ggsave(filename = paste0("results/", 
                                plot_prefix, 
                                "_figS3_v2.",.x),
              plot = figS3,
              width = 210, height = 280, units="mm",
              dpi = 320))
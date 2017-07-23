## Notes for running Zika estimation in Texas

1. Start with `analyze_temperature_data.R`, which shows how to download historic temperature data and get averages for each county
2. Move to `generate_scam_list.R` to get monte carlo samples for R0 estimation parameters
3. Once scam lists are generated, need to run `calc_monthly_rnots.R` to get R0 distributions for each county and month
4. Run the `calc_dispersion_table.R` file to setup parameters that will be needed during the estimation procedures
5. Run the `calc_alpha_posterior.R` file for each parameter set (reporting rate and secondary transmission number in November)
      - `create_alpha_like_job_file.R` is script that can create a job script for running this step on high performance computing
6. `calc_alpha_posterior.R` will then convert the posterior alpha data into usable formats for plotting
7. `calc_final_posterior_rnots.R` file will obtain final posterior R0s for each county and month and convert to format for plotting
8. run `fake_mcmc_dat_generator.R` to get the fake data used for the first figure
9. run `ms_fig_creator.R`, which can be used to make all of the figures from the manuscript

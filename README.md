## Notes for running Zika estimation in Texas

1. Start with `analyze_temperature_data.R`, which shows how to download historic temperature data and get averages for each county
2. Move to `generate_scam_list.R` to get monte carlo samples for R0 estimation parameters
3. Once scam lists are generated, need to run `calc_monthly_rnots.R` to get R0 distributions for each county and month
4. Run the `calc_dispersion_table.R` file to obtain the dispersion parameter to be used for the fitting
      - Take the number (printed minimum), and substitute it into the `cpp_fitting_fxns.R` file in the `find_rnot_ods()` function in place of the number specified there if different than 0.12

5. Run the posterior scaling factor estimation to get posteriors at every time point for the scaling factor
      - Run `create_alpha_like_job_file.R` To create job file for running posterior estimationg for multiple parameters (temperature used, reporting rate, and secondary transmission number in November)
          - This file creates a file where each line is a "job" that can be run using the command line to call an R script
          - If you don't have High performance computing capability to schedule jobs, you could alternatively run each of these lines manually from your own terminal, or create a different R script that accomplishes the same tasks.
      - Run the `calc_alpha_posterior.R` file for each parameter set in the job file (I do this with slurm script in Wrangler on TACC (https://www.tacc.utexas.edu/))
          - See slurm script for my specifications for running file, but may not work on your HPC machine
6. Run `calc_final_posterior_rnots.R`, which will run single MCMC for each parameter set, and save posterior distributions for all county R0s in a usable format for plotting.
      
7. `calc_alpha_posterior.R` will then convert the posterior alpha data into usable formats for plotting

8. run `fake_mcmc_dat_generator.R` to get the fake data used for the first figure
9. run `cty_sec_trans.R` to get data for last figure in ms
10. run `ms_fig_creator.R`, which can be used to make all of the figures from the manuscript
